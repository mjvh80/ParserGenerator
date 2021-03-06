﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Diagnostics;

// haskell port to c#

namespace SimpleRegexIntersector
{
   // todo: also used elsewhere, put in lib
   internal static class _Extensions
   {
      public static T[] Merge<T>(this T[] left, T[] right)
      {
         if (left.Length == 0)
            return right;
         if (right.Length == 0)
            return left;
         T[] tResult = new T[left.Length + right.Length];
         Array.Copy(left, tResult, left.Length);
         Array.Copy(right, 0, tResult, left.Length, right.Length);
         return tResult;
      }

      // reuses left!
      public static HashSet<T> Merge<T>(this HashSet<T> left, HashSet<T> right)
      {
         foreach (T tItem in right)
            left.Add(tItem);
         return left;
      }

      public static Char Increment(this Char c)
      {
         return (c += '\x0001');
      }

      public static Char Decrement(this Char c)
      {
         return (c -= '\x0001');
      }

      public static void AddRange<T>(this HashSet<T> set, IEnumerable<T> range)
      {
         foreach (T tItem in range)
            set.Add(tItem);
      }

      /// <summary>
      /// Deduplicates the array.
      /// </summary>
      public static T[] Nub<T>(this T[] array)
      {
         // for now, naive implementation
         HashSet<T> tSet = new HashSet<T>();
         foreach (T tItem in array)
            tSet.Add(tItem);

         if (tSet.Count == array.Length)
            return array;
         else
         {
            return tSet.ToArray();
         }
      }

      /// <summary>
      /// Deduplicates the given list.
      /// </summary>
      public static List<T> Nub<T>(this List<T> list)
      {
         // for now, naive implementation
         HashSet<T> tSet = new HashSet<T>();
         foreach (T tItem in list)
            tSet.Add(tItem);

         if (tSet.Count == list.Count)
            return list;
         else
         {
            List<T> tResult = new List<T>();
            tResult.AddRange(tSet);
            return tResult;
         }
      }
   }

   // todo: look at replacing this with Tuples.
   public class Pair<U, V> 
   { 
      public U Left; public V Right;

      public override bool Equals(object obj)
      {
         Pair<U, V> tOther = obj as Pair<U, V>;
         return tOther != null && Left.Equals(tOther.Left) && Right.Equals(tOther.Right);
      }

      public override int GetHashCode()
      {
         return Left.GetHashCode() ^ Right.GetHashCode();
      }
   }

   public class Pair : Pair<SimpleRegex, SimpleRegex> { }

   public class SimpleRegexBuilder
   {
      public static SimpleRegexBuilder Default = new SimpleRegexBuilder().Build();

      protected Boolean mBuilt = false;

      internal HashSet<Char> mAlphabet;
      internal HashSet<RangeRegex> mRangeSet;

      protected Dictionary<RangeRegex, Char> mRangeMarkerMap = new Dictionary<RangeRegex, char>();
      protected readonly Char mMagicMarker = '\uFADE'; // todo: must choose something dynamically
         

      public SimpleRegexBuilder() : this(null, null) { }

      public SimpleRegexBuilder(HashSet<Char> alphabet, HashSet<RangeRegex> rangeSet) // todo: i think another type for ranges here may be better
      {
         if (alphabet == null)
            mAlphabet = new HashSet<char>();
         else
            mAlphabet = new HashSet<char>(alphabet);

         if (rangeSet == null)
            mRangeSet = new HashSet<RangeRegex>();
         else
            mRangeSet = new HashSet<RangeRegex>(rangeSet);
      }

      public void AddLetter(Char letter)
      {
         if (mBuilt)
            throw new InvalidOperationException("SimpleRegexBuilder: can not add letter when built");
         mAlphabet.Add(letter);
      }

      public void AddRange(RangeRegex range)
      {
         if (mBuilt)
            throw new InvalidOperationException("SimpleRegexBuilder: can not add range when built");
         mRangeSet.Add(range);
      }

      public SimpleRegexBuilder Build()
      {
         if (mBuilt)
            throw new InvalidOperationException("SimpleRegexBuilder: already built");

#if DEBUG
         if (mRangeSet != null)
            foreach (RangeRegex tRange in mRangeSet)
               if (!mAlphabet.Contains(tRange.Low) || !mAlphabet.Contains(tRange.High))
                  throw new InvalidOperationException("invalid range: boundaries not included in alphabet");

         if (mAlphabet.Contains(mMagicMarker)) // todo: must ensure this never happens, this is a safety catch for now
            throw new Exception("magic marker is used");
#endif

         mRangeSet = CreateDisjointRangeSet(mRangeSet);

         // Update the alphabet.
         foreach (RangeRegex tRange in mRangeSet)
         {
            mAlphabet.Add(tRange.Low);
            mAlphabet.Add(tRange.High);
         }

         // Build marker map.
         Char tMarkerStart = 'A';// '\uf000'; // say, must find something "proper" todo
         foreach (RangeRegex tDisjointRange in mRangeSet)
         {
            if (tMarkerStart == '\uffff')
               throw new Exception("Markers exhausted"); // this is bad, but itll have to do for now..

            mRangeMarkerMap.Add(tDisjointRange, tMarkerStart += '\x0001');
         }

         mBuilt = true;
         return this;
      }

      protected static HashSet<RangeRegex> CreateDisjointRangeSet(IEnumerable<RangeRegex> rangeSet)
      {
         HashSet<RangeRegex> tDisjointSet = new HashSet<RangeRegex>();
         if (rangeSet != null)
            foreach (RangeRegex tRange in rangeSet)
            {
               SimpleRegex.AddRangeDisjoint(tRange.Negated ? tRange.Invert() : tRange, tDisjointSet); // todo: put this method somewhere else?
            }
         return tDisjointSet;
      }

      protected void EnsureBuilt()
      {
         if (!mBuilt)
            throw new InvalidOperationException("SimpleRegexBuilder: not built");
      }

      public Boolean IsBuilt { get { return mBuilt; } }

      public SimpleRegex Parse(String regexExpr)
      {
         EnsureBuilt();
         SimpleRegex tResult = SimpleRegex.Parse(regexExpr);
         return Normalize(tResult);
      }

      protected SimpleRegex Normalize(SimpleRegex targetRegex)
      {
         EnsureBuilt();

         if (mAlphabet.Count == 0) // nothing to normalize against
            return targetRegex;

         SimpleRegex tResult = targetRegex;

#if DEBUG
         foreach (Char tChar in tResult.Sigma())
            if (!mAlphabet.Contains(tChar))
               throw new InvalidOperationException("char " + tChar + " not found in alphabet");
#endif

         // Process ranges by removing any characters used elsewhere.
         // [a-d] -> [a-b] | c | d if c and d used elsewhere, with negations appropriately applied.
         // Also, remove ranges of length 1.

         // todo: the negation here is incorrect as it can contain empty and it should not!
         tResult = tResult.Apply(r =>
         {
            if (r is RangeRegex)
            {
               RangeRegex tRange = (RangeRegex)r;
               var tSplitRanges = SimpleRegex.SplitOnLetters(tRange, mAlphabet);
               SimpleRegex tRewrite;
               if (tRange.Negated)
                  tRewrite = SimpleRegex.And(from tSplitRange in tSplitRanges
                                             select tSplitRange.Length == 1 ?
                                                (SimpleRegex)new NegatedLetterRegex { Letter = tSplitRange.Low }
                                                : tSplitRange.Invert());
               else
                  tRewrite = SimpleRegex.Choice(from tSplitRange in tSplitRanges
                                             select tSplitRange.Length == 1 ?
                                                (SimpleRegex)Letter(tSplitRange.Low) : tSplitRange);

               //return tRange.Negated ? tRewrite.Negate() : tRewrite;
               return tRewrite;
            }
            return r;
         });

         // 3. Rewrite regex tree with all ranges, rewriting with markers.
         tResult = tResult.Apply(r =>
         {
            if (r is RangeRegex)
               return SimpleRegex.Choice(from recon in SimpleRegex.Reconstruct((RangeRegex)r, mRangeSet)
                             select
                                ((RangeRegex)r).Negated ?
                                (SimpleRegex)SimpleRegex.NegatedLetter(mRangeMarkerMap[recon]) :
                                SimpleRegex.Letter(mRangeMarkerMap[recon]));

            return r;
         });

         // Rewrite all negations, and complete regexes (dot) using the magic marker.
        
         // 1. Rewrite Complete regex:
         // . = toregex(used_chars)
         // If string s matches R1 and R2, and R1 contains '.' then in R2 it must either:
         // - match a char in the regex (thus in toregex(used))
         // - match . -> can rewrite s and replace the character at the position that matched with one of used.
         // This works for intersection, but not for semantic equality i think.
         // 
         // Create a map from not-char to replacement.

         // Before we get out, rewrite ., matching everything.
         tResult = RewriteComplete(tResult);

         // Rewrite negated letters.
         tResult = RewriteNegation(tResult);

         return tResult;
      }

      protected SimpleRegex RewriteComplete(SimpleRegex regex)
      {
         return regex.Apply(r =>
         {
            if (r is CompleteRegex)
               return SimpleRegex.Choice(from c in mAlphabet select Letter(c), Letter(mMagicMarker));
            return r;
         });
      }

      protected SimpleRegex RewriteNegation(SimpleRegex regex)
      {
         return regex.Apply(r =>
         {
            if (r is NegatedLetterRegex)
               return SimpleRegex.Choice(from c in mAlphabet where c != ((NegatedLetterRegex)r).Letter select Letter(c), Letter(mMagicMarker));
            return r;
         });
      }



      // We'll only provide methods here that do *not* need additional rewriting (todo: provided all are normalized).
      public static ChoiceRegex Choice(SimpleRegex left, SimpleRegex right) { return new ChoiceRegex() { Left = left, Right = right }; }
      public static KleeneRegex Star(SimpleRegex op) { return new KleeneRegex() { Operand = op }; }
      public static SeqRegex Sequence(SimpleRegex left, SimpleRegex right) { return new SeqRegex() { Left = left, Right = right }; }
     
      public static LetterRegex Letter(Char c) { return new LetterRegex() { Letter = c }; }

      // todo: look at instance vs static here.. bit of a mess
      // the one below requires rewriting
      // does NOT clone
      public SimpleRegex AndNot(SimpleRegex arg, SimpleRegex op) // todo: should ensure both have been normalized....
      {
         // Logic: left followed by ~op .*
         //return (new AndRegex() { Left = arg, Right = Sequence(RewriteNegation(op.Negate()), Star(RewriteComplete(SimpleRegex.Complete))) });
         return new AndRegex { Left = arg, Right = RewriteNegation(op.Negate()) };
      }


      // These would, do not support for now.

      //public static VarRegex Var(Int32 x) { return new VarRegex() { Value = x }; }
      //public static NegatedLetterRegex NegatedLetter(Char c) { return new NegatedLetterRegex() { Letter = c }; }
      //public static AndRegex And(SimpleRegex left, SimpleRegex right) { return new AndRegex() { Left = left, Right = right }; }
      //public static SimpleRegex Not(SimpleRegex op) { return op.Negate(); }
      //public static RangeRegex Range(Char lo, Char hi) { return new RangeRegex() { Low = lo, High = hi }; }
   }

   // todo: other extensions possible:
   // ^: match start of input: requires no work, just ensure with parsing that ^ can not occur as char
   // $: match end of input: - " -

   public interface IRegexVisitor<out T>
   {
      T Visit(ChoiceRegex choice);
      T Visit(SeqRegex sequence);
      T Visit(LetterRegex letter);
      T Visit(NegatedLetterRegex negLet);
      T Visit(AndRegex and);
      T Visit(EmptyRegex empty);
      T Visit(ZeroRegex zero);
      T Visit(VarRegex var);
      T Visit(CompleteRegex complete);
      T Visit(KleeneRegex kleene);
      T Visit(RangeRegex range);
   }

   // Visits the complete tree of regex, replacing nodes with the visitor value.
   internal class SimpleVisitor : IRegexVisitor<SimpleRegex>
   {
      public virtual SimpleRegex Visit(ChoiceRegex choice)
      {
         choice.Left = choice.Left.Accept(this);
         choice.Right = choice.Right.Accept(this);
         return choice;
      }

      public virtual SimpleRegex Visit(SeqRegex sequence)
      {
         sequence.Left = sequence.Left.Accept(this);
         sequence.Right = sequence.Right.Accept(this);
         return sequence;
      }

      public virtual SimpleRegex Visit(LetterRegex letter)
      {
         return letter;
      }

      public virtual SimpleRegex Visit(RangeRegex range)
      {
         return range;
      }

      public virtual SimpleRegex Visit(NegatedLetterRegex negLet)
      {
         return negLet;
      }

      public virtual SimpleRegex Visit(AndRegex and)
      {
         and.Left = and.Left.Accept(this);
         and.Right = and.Right.Accept(this);
         return and;
      }

      public virtual SimpleRegex Visit(EmptyRegex empty)
      {
         return empty;
      }

      public virtual SimpleRegex Visit(ZeroRegex zero)
      {
         return zero;
      }

      public virtual SimpleRegex Visit(VarRegex var)
      {
         return var;
      }

      public virtual SimpleRegex Visit(CompleteRegex complete)
      {
         return complete;
      }

      public virtual SimpleRegex Visit(KleeneRegex kleene)
      {
         kleene.Operand = kleene.Operand.Accept(this);
         return kleene;
      }
   }

   internal class SimplifyingVisitor : SimpleVisitor
   {
      public override SimpleRegex Visit(ChoiceRegex choice)
      {
         choice = (ChoiceRegex)base.Visit(choice);

         if (choice.Left.IsZero()) // 0 | A = A
            return choice.Right;

         if (choice.Right.IsZero()) // A | 0 = A
            return choice.Left;

         if (choice.Left.Equals(choice.Right))
            return choice.Left;

         return choice;
      }

      public override SimpleRegex Visit(SeqRegex sequence)
      {
         sequence = (SeqRegex)base.Visit(sequence);

         if (sequence.Left.IsZero() || sequence.Right.IsZero()) // 0 A = 0 and A 0 = 0
            return SimpleRegex.Zero;

         // empty A = A
         if (sequence.Left == SimpleRegex.Empty)
            return sequence.Right;

         // A empty = A
         if (sequence.Right == SimpleRegex.Empty)
            return sequence.Left;

         return sequence;
      }

      public override SimpleRegex Visit(AndRegex and)
      {
         and = (AndRegex)base.Visit(and);

         // A & 0 = 0
         if (and.Left.IsZero())
            return SimpleRegex.Zero;

         // 0 & A = 0
         if (and.Right.IsZero())
            return SimpleRegex.Zero;

         // empty & A = empty
         if (and.Left == EmptyRegex.Instance)
            return EmptyRegex.Instance;

         if (and.Right == EmptyRegex.Instance)
            return EmptyRegex.Instance;

         if (and.Left.Equals(and.Right))
            return and.Left;

         return and;
         //return new AndRegex { Left = tLeft, Right = tRight }; // enough for now
      }

      public override SimpleRegex Visit(KleeneRegex kleene)
      {
         kleene = (KleeneRegex)base.Visit(kleene);

         // 0* = empty
         if (kleene.Operand == SimpleRegex.Zero) 
            return SimpleRegex.Empty;

         // empty* = empty
         if (kleene.Operand == SimpleRegex.Empty)
            return SimpleRegex.Empty;

         return kleene;
      }
   }

   /// <summary>
   /// Represents a .NET regular expression, but then a more "simple" one, so not all syntax supported and it doesn't perform
   /// any matching. It provides functionality such as:
   /// - testing equivalence of regular expressions
   /// - testing whether they share a common prefix (ie would match the same prefix string in all its input).
   /// Class is immutable so operations such as Negate etc. returns copies (in this case a regular expression that matches anything
   /// this one does not).
   /// </summary>
   public abstract class SimpleRegex
   {
      internal static readonly Char[] EmptyCharArray = new Char[] { };

      ///<summary>Matches the empty string.</summary> 
      internal static readonly SimpleRegex Empty = EmptyRegex.Instance;

      ///<summary>Matches nothing.</summary>
      internal static readonly SimpleRegex Zero = ZeroRegex.Instance; // new ZeroRegex();

      /// <summary>
      /// Matches everything (.).
      /// </summary>
      internal static readonly SimpleRegex Complete = CompleteRegex.Instance; // new CompleteRegex();

      /// <summary>
      /// Returns, for regex f, the set { p | (x, p) in linear forms of f }.
      /// </summary>
      /// <param name="c"></param>
      /// <returns></returns>
      internal abstract HashSet<SimpleRegex> PartialDeriv(Char c);
      internal abstract Char[] Sigma();
      internal abstract Boolean IsEmpty();
      internal abstract Boolean IsZero();

      /// <summary>
      /// Clones this regex.
      /// </summary>
      /// <returns></returns>
      public abstract SimpleRegex Clone();

      /// <summary>
      /// Returns a regex that is negated, so matches whatever this does not match.
      /// Current regex is *not* affected, a copy is always returned.
      /// </summary>
      /// <returns></returns>
      internal abstract SimpleRegex Negate();

      public abstract T Accept<T>(IRegexVisitor<T> visitor);


      /// <summary>
      /// Returns a new regex that is simplified. For example zero | A = A etc.
      /// Currently not used internally.
      /// </summary>
      /// <returns></returns>
      // todo: should we simplify more "accross the board" in order to save work etc.?
      public SimpleRegex Simplify()
      {
         return this.Accept(new SimplifyingVisitor());
      }

      // Util for rewriting the tree. todo: should provide viistor instead?
      public abstract SimpleRegex Apply(Func<SimpleRegex, SimpleRegex> f);
      
      // Helpers to combine regexes. Note: no cloning is preformed (!).
      internal static Pair Pair(SimpleRegex left, SimpleRegex right) { return new Pair() { Left = left, Right = right }; }
      
      public static ChoiceRegex Choice(SimpleRegex left, SimpleRegex right) { return new ChoiceRegex() { Left = left, Right = right }; }
      public static KleeneRegex Star(SimpleRegex op) { return new KleeneRegex() { Operand = op }; }
      public static SeqRegex Sequence(SimpleRegex left, SimpleRegex right) { return new SeqRegex() { Left = left, Right = right }; }
      public static VarRegex Var(Int32 x) { return new VarRegex() { Value = x }; }
      public static LetterRegex Letter(Char c) { return new LetterRegex() { Letter = c }; }
      public static NegatedLetterRegex NegatedLetter(Char c) { return new NegatedLetterRegex() { Letter = c }; }
      public static AndRegex And(SimpleRegex left, SimpleRegex right) { return new AndRegex() { Left = left, Right = right }; }
      public static SimpleRegex Not(SimpleRegex op) { return op.Negate(); }
      public static RangeRegex Range(Char lo, Char hi) { return new RangeRegex() { Low = lo, High = hi }; }

      // todo: force not negated?
      public HashSet<RangeRegex> GetClonedRanges()
      {
         // todo ..
         HashSet<RangeRegex> tRanges = new HashSet<RangeRegex>();
         this.Apply(s => { if (s is RangeRegex) tRanges.Add((RangeRegex)s.Clone()); return s; });
         return tRanges;
      }

      // Public api: todo consolidate?
      public Char[] Letters()
      {
         return this.Sigma();
      }


      /* Performs a conversion to remove recursion:
       * 
       * (r) x (r1) = r2 => x =?
       * 
       * ie A ^ B = R1 x | R2 = R1 (A ^ B) | R2 => x = R1* R2 (see Choice).
       * 
       * x, e -> (e, e)
       * x, var(x) -> (e, e)
       * x, var(y) -> (e, var(y)) <-- no change, we're not converting this one
       * x, seq(a,b) -> if (x in b):
       *               (a2, b2) = convert(x, b)
       *                  Seq(a, a2), b2              => (a a2, b2)
       *               else
       *                  (e, r)                      => (e, a b)    <- no change
       * x, choice(a, b) -> (choice(a1, b1), choice(a2, b2)) => (a1 | b1), (a2 | b2)
       * x, star(a) -> (e, star(a)) => (e, a*)
       * 
       * Actual conversion is:
       * 
       * r -> Seq(star r1) r2 => r1* r2
       * 
       * See Choice and other implementations for more on this.
       * 
       * Return: matches of the regex containing x on the left, others on the right.
       */
      internal abstract Pair ConvertInternal(Int32 x);
      internal abstract Boolean Contains(Int32 x);

      // Internally call ConvertInternal, which returns a pair.
      // If the regex does not contain x, this will return { empty, this }.
      // This then returns empty* this = this.
      //
      // This solves a fixed point equation. Given two regexes A, B: A intersect B = x = f(x).
      // 
      // a x -> (a empty)* empty* -> a*, a x = x = a+
      internal SimpleRegex Convert(Int32 x)
      {  
         Pair tConversion = this.ConvertInternal(x);
         return Sequence(Star(tConversion.Left), tConversion.Right);
      }

      ///// <summary>
      ///// Public method to create a choice regex. Clones all regexes in the list.
      ///// </summary>
      ///// <param name="pList"></param>
      ///// <param name="others"></param>
      ///// <returns></returns>
      //public static SimpleRegex NewChoice(IEnumerable<SimpleRegex> pList, params SimpleRegex[] others)
      //{
      //   return ToRegex(pList.Union(others).Select(n => n.Clone()).GetEnumerator());
      //}

      /// <summary>
      /// Internal method to create a choice, does not clone.
      /// </summary>
      /// <remarks>Does not clone. Should not need to, as it should not change its children.</remarks>
      public static SimpleRegex Choice(IEnumerable<SimpleRegex> pList, params SimpleRegex[] others)
      {
         //return ToRegex(pList.GetEnumerator());
         return ToRegex((pList.Union(others).GetEnumerator()));
      }

      public static SimpleRegex And(IEnumerable<SimpleRegex> pList)
      {
         return And(pList.GetEnumerator());
      }

      public static SimpleRegex And(IEnumerator<SimpleRegex> regexList)
      {
         if (!regexList.MoveNext())
            return Zero;

         SimpleRegex tLeft = regexList.Current;

         SimpleRegex tRest = And(regexList);

         return tRest == Zero ? tLeft : And(tLeft, tRest);
      }

      /// <summary>
      /// If list is empty, returns Zero. Otherwise returns a choice of the list, finally terminated with Zero (!).
      /// Thus, this is always "terminated" with Zero.
      /// Performs no cloning, internal method.
      /// </summary>
      /// <param name="pList"></param>
      /// <returns></returns>
      internal static SimpleRegex ToRegex(IEnumerator<SimpleRegex> pList)
      {
         if (pList.MoveNext())
            return Choice(pList.Current, ToRegex(pList));
         else
            return Zero;
      }

      // todo: compare a|b with b|a gives error...
      // note: does not rewrite, and can change the 
      public Boolean Intersects(SimpleRegex other)
      {
         // Second change to MS' code: compare with Zero as well:
         SimpleRegex tIntersection = this.Intersect(new Dictionary<Pair, SimpleRegex>(), 0, other);
         return !(Empty.SemanticEquals(tIntersection) || Zero.SemanticEquals(tIntersection));
      }

      public Boolean SharesCommonPrefixWith(SimpleRegex other)
      {
         // Second change to MS' code: compare with Zero as well:
         SimpleRegex tPrefixIntersection = this.GetCommonPrefix(new Dictionary<Pair, SimpleRegex>(), 0, other);
         return !(Empty.SemanticEquals(tPrefixIntersection) || Zero.SemanticEquals(tPrefixIntersection));
      }

      internal SimpleRegex Intersect(SimpleRegex other)
      {
         return this.Intersect(new Dictionary<Pair, SimpleRegex>(), 0, other);
      }

      internal SimpleRegex Intersect(Dictionary<Pair, SimpleRegex> env, Int32 x, SimpleRegex r2)
      {
         if (this == Zero || r2 == Zero)
            return Zero;

         if (this == Empty || r2 == Empty)
            return Empty; // shouldn't this be Zero?

         SimpleRegex tEntry;
         if (env.TryGetValue(Pair(this, r2), out tEntry))
         {
            // already seen, don't do it again
            return tEntry;
         }
         else
         {
            Char[] tLetters = Choice(this, r2).Sigma();
            env.Add(Pair(this, r2), Var(x));
            Func<Char, SimpleRegex> r1c = c => Choice(this.PartialDeriv(c));
            Func<Char, SimpleRegex> r2c = c => Choice(r2.PartialDeriv(c));
            List<SimpleRegex> tResult = new List<SimpleRegex>(tLetters.Length);
            foreach (Char tChar in tLetters)
               tResult.Add(Sequence(Letter(tChar), r1c(tChar).Intersect(env, x + 1, r2c(tChar))));
            SimpleRegex tFinal;
            if (this.IsEmpty() && r2.IsEmpty())
               tFinal = Choice(Choice(tResult), Empty); // empty is in the intersection
            else
               tFinal = Choice(tResult);

            // Remove the regex for this recursion level. Adjusts M.S. algorithm.
            env.Remove(Pair(this, r2));

            // Convert and return ("solves" for the regex variables).
            return tFinal.Convert(x);
         }
      }

      // Force implentation.
      public override abstract int GetHashCode();
      public override abstract bool Equals(Object other);

      // R = A B
      internal SimpleRegex GetCommonPrefix(Dictionary<Pair, SimpleRegex> env, Int32 x, SimpleRegex r2)
      {
         // If any one is empty/zero, we should stop processing: we found our prefix.
         // Return empty, so that the prefix found does not match zero.
         if (this == Zero || r2 == Zero)
            return Empty;

         if (this == Empty || r2 == Empty)
            return Empty;

         SimpleRegex tEntry;
         if (env.TryGetValue(Pair(this, r2), out tEntry))
         {
            // already seen, don't do it again
            return tEntry;
         }
         else
         {
            Char[] tLetters = Choice(this, r2).Sigma();
            env.Add(Pair(this, r2), Var(x));
            Func<Char, SimpleRegex> r1c = c => Choice(this.PartialDeriv(c));
            Func<Char, SimpleRegex> r2c = c => Choice(r2.PartialDeriv(c));
            List<SimpleRegex> tResult = new List<SimpleRegex>(tLetters.Length);
            foreach (Char tChar in tLetters)
            {
               SimpleRegex tLeft = r1c(tChar);
               SimpleRegex tRight = r2c(tChar);
               // (!Zero.SemanticEquals(tLeft) && !Zero.SemanticEquals(tRight))
               // If there is a non-zero partial derivative, there is a common prefix starting with tChar.
               if (!tLeft.IsZero() && !tRight.IsZero()) // note: isZero checks if it IS zero, not CONTAINS
                  tResult.Add(Sequence(Letter(tChar), tLeft.GetCommonPrefix(env, x + 1, tRight)));
            }
            SimpleRegex tFinal;
            // If any is empty ("starts with empty"), empty is a result.
           // if (this.IsEmpty() || r2.IsEmpty())
               tFinal = Choice(Choice(tResult), Empty); // empty is a common prefix
            //else
            //   tFinal = Choice(tResult);

            // Remove the regex for this recursion level. Adjust M.S. algorithm.
            env.Remove(Pair(this, r2));
            return tFinal.Convert(x);
         }
      }

      protected Boolean SemanticEquals(HashSet<Pair> env, SimpleRegex r2)
      {
         if (this.IsEmpty() && !r2.IsEmpty())
            return false;

         if (this.IsZero() && !r2.IsZero())
            return false;

         // If we have seen this pair before, they must be equal.
         // If not there is a point in the foreach where this is called, and false returned and so on.
         if (env.Contains(Pair(this, r2)))
            return true;

         Char[] tLetters = Choice(this, r2).Sigma();
         env.Add(Pair(this, r2));
         foreach (Char tLetter in tLetters)
            if (!Choice(this.PartialDeriv(tLetter)).SemanticEquals(env, Choice(r2.PartialDeriv(tLetter))))
               return false;
         return true;
      }

      public Int32 EqualsConsistentHashCode()
      {
         return EqualsConsistentHashCode(new Dictionary<SimpleRegex, Int32>(), 1);
      }

      // Every regex is:
      // Sum(partial_derivs) | empty | zero
      // Can we do better?
      // this is shitty.. and maps too many hashcodes on one, must improve..
      // todo: must seriously do better here

      // We may have an issue with a recursive call here, ie 
      // hashcode h = f(h), some function of itself.
      // So, we need some fixed point, but not sure how yet.
      // NOTE: for now this uses only the letters of the regex, this means you need to used NORMALIZED regular expressions, ie, use the builder in order
      // to have rewritten regular expressions. This ensures that a regex such as (a|[^a])b has the same hashcode as .b etc.
      // todo: make his internal and expose through builder instead to emphasize fact normalization needed?
      protected Int32 EqualsConsistentHashCode(Dictionary<SimpleRegex, Int32> env, Int32 cnt)
      {
         // No effect on hashocde
         if (this.IsZero())
            return 103093;

         if (this == Empty)
            return 54799;

         Char[] tLetters = Sigma();

         // This should not depend on letters used, as any two regexes that are equal don't necessarily have the same letters.
         // note, however, that are regexes are rewritten.
         // Array.Sort(tLetters); // don't need to sort, ^ is commutative and associative

         if (env.ContainsKey(this))
            return env[this];

         Int32 tResult = cnt;
         // uncomment, if using below recursive call (!)
         //env[this] = 0;

         foreach (Char tLetter in tLetters)
            tResult ^= tLetter.GetHashCode();

         // note: the below is incorrect
         // the recursive call means we really need to find a fixed point, but i don't yet know how

         // The idea here is to follow equals, by rewriting as choice of partial derivs, then calculating their hashcodes.
         // This should give two equal regexes equal hashcodes.

         //Int32 x = 0;
         //foreach (Char tLetter in tLetters)
         //{
         //   // old
         //   //tResult ^= tLetter.GetHashCode();
         //   //tResult ^= Choice(PartialDeriv(tLetter)).EqualsConsistentHashCode(env, cnt);
            
         //   // new
         //   //Int32 x = 0;
         //   //foreach (SimpleRegex tRegex in PartialDeriv(tLetter))
         //   //   x ^= tRegex.EqualsConsistentHashCode(env, cnt);   
         //}

         //tResult ^= x;

         return tResult ^ tLetters.Length;
      }

      public Boolean SemanticEquals(SimpleRegex other)
      {
         return SemanticEquals(new HashSet<Pair>(), other);
      }

      protected static Char Min(Char left, Char right)
      {
         return left < right ? left : right;
      }

      protected static Char Max(Char left, Char right)
      {
         return left > right ? left : right;
      }

      internal static IEnumerable<RangeRegex> Split(RangeRegex range, RangeRegex comp)
      {
         if (!range.OverlapsWith(comp))
            throw new InvalidOperationException("ranges don't overlap");

         if (range.Equals(comp))
            yield return range;

         Char tMin, tMax;
         tMin = Min(range.Low, comp.Low);
         tMax = Max(range.Low, comp.Low);
         yield return Range(tMin, tMax); // min(low) , max (low)
         tMax += '\x0001';
         tMin = Min(range.High, comp.High);
         if (tMax <= tMin)
            yield return Range(tMax, tMin); // max(low) + 1 , min(high)
         tMin += '\x0001';
         tMax = Max(range.High, comp.High);
         if (tMin <= tMax)
            yield return Range(tMin, tMax); // min(high) + 1, max(high)
      }

      // given a range and a set of disjoint ranges:
      // 1. if it is disjoint with the entire set: add it
      // 2. otherwise, for each intersection, spilt up ranges and update disjoint set.
      internal static void AddRangeDisjoint(RangeRegex range, HashSet<RangeRegex> disjointSet)
      {
         if (disjointSet.Count == 0)
            disjointSet.Add(range);
         else if (disjointSet.Contains(range))
            return;
         else
         {
            HashSet<RangeRegex> tReplaceSet = new HashSet<RangeRegex>();
            foreach (RangeRegex tDisjoint in disjointSet)
               if (range.OverlapsWith(tDisjoint))
                  tReplaceSet.Add(tDisjoint);

            if (tReplaceSet.Count == 0)
               disjointSet.Add(range);
            else
               foreach (RangeRegex tReplacement in tReplaceSet)
               {
                  disjointSet.Remove(tReplacement);
                  foreach (RangeRegex tDisjoint in Split(range, tReplacement))
                     AddRangeDisjoint(tDisjoint, disjointSet);
               }
         }
      }

      internal static IEnumerable<RangeRegex> Reconstruct(RangeRegex range, HashSet<RangeRegex> disjointRanges)
      {
         foreach (RangeRegex tDisjointRange in disjointRanges)
            if (range.Contains(tDisjointRange))
               yield return tDisjointRange;
      }

      // again, implemented naively, ill focus on some perf later...
      // to improve: used a sorted list of letters and iterate through once
      internal static IEnumerable<RangeRegex> SplitOnLetters(RangeRegex range, IEnumerable<Char> letters)
      {
         Boolean tSplit = false;
         foreach(Char tLetter in letters)
            if (range.Contains(tLetter))
            {
               if (tLetter > range.Low)
                  foreach (RangeRegex tRange in SplitOnLetters(Range(range.Low, tLetter.Decrement()), letters))
                     yield return tRange;

               yield return Range(tLetter, tLetter);

               if (tLetter < range.High)
                  foreach (RangeRegex tRange in SplitOnLetters(Range(tLetter.Increment(), range.High), letters))
                     yield return tRange; // this unwrapping should be automatic...

               tSplit = true;
               break; // solved recursively
            }

         if (!tSplit)
            yield return Range(range.Low, range.High); // ie NOT (necessarily) negated
            //yield return range;
      }

      // Rewriting support for ranges and negation.
      // Does not clone, changes tree.
      // todo: instead, clone here rather than above?
      [Obsolete("to be removed", true)]
      internal SimpleRegex Rewrite()
      {
         SimpleRegex tResult = this;

         HashSet<Char> tUsedSet = new HashSet<char>();
         tUsedSet.AddRange(tResult.Sigma());

         // Process ranges by removing any characters used elsewhere.
         // [a-d] -> [a-b] | c | d if c and d used elsewhere, with negations appropriately applied.
         // Also, remove ranges of length 1.
         tResult = tResult.Apply(r =>
         {
            if (r is RangeRegex)
            {
               RangeRegex tRange = (RangeRegex)r;
               var tSplitRanges = SplitOnLetters(tRange, tUsedSet);
               SimpleRegex tRewrite = Choice(from tSplitRange in tSplitRanges
                                        select tSplitRange.Length == 1 ?
                                           (SimpleRegex)Letter(tSplitRange.Low) : tSplitRange);

               // TODO: thdis is no longer correct, see builder
               return tRange.Negated ? tRewrite.Negate() : tRewrite;
            }
            return r;
         });

         // Update characters, may have added some.
         tUsedSet.AddRange(tResult.Sigma());


         // Process Ranges: rewriting with marker characters.

         // 1. Collect all ranges.
         HashSet<RangeRegex> tRanges = new HashSet<RangeRegex>();
         tResult.Apply(r =>
         {
            if (r is RangeRegex)
               tRanges.Add((RangeRegex)r);
            return r;
         
         });
         // 2. Rewrite ranges to be disjoint.
         HashSet<RangeRegex> tDisjointRanges = new HashSet<RangeRegex>();
         foreach (RangeRegex tRange in tRanges)
            AddRangeDisjoint(tRange, tDisjointRanges);
         
         // 3. Create map from disjoint range to marker.
         Dictionary<RangeRegex, Char> tRangeMarkerMap = new Dictionary<RangeRegex, char>();
         Char tMarkerStart = 'A';// '\uf000'; // say, must find something "proper"
         foreach (RangeRegex tDisjointRange in tDisjointRanges)
         {
            if (tMarkerStart == '\uffff')
               throw new Exception("Markers exhausted"); // this is bad, but itll have to do for now..

            tRangeMarkerMap.Add(tDisjointRange, tMarkerStart += '\x0001');
         }
         
         // 4. Rewrite regex tree with all ranges, rewriting with markers.
         tResult = tResult.Apply(r =>
         {
            // todo: no longer correct, see builder
            if (r is RangeRegex)
               return Choice(from recon in Reconstruct((RangeRegex)r, tDisjointRanges) select
                                 ((RangeRegex)r).Negated ? 
                                    //(SimpleRegex)NegatedLetter(tRangeMarkerMap[recon]) :
                                    (SimpleRegex)Letter(tRangeMarkerMap[recon]).Negate() :
                                    (SimpleRegex)Letter(tRangeMarkerMap[recon]));

            return r;
         });

         // Again, update all characters: can probably be bit more clever about this.
         tUsedSet.AddRange(tResult.Sigma());

         // Rewrite all negations, and complete regexes (dot) using the magic marker.
         Char tMagicMarker = '\uFADE';

         // 1. Rewrite Complete regex:
         // . = toregex(used_chars)
         // If string s matches R1 and R2, and R1 contains '.' then in R2 it must either:
         // - match a char in the regex (thus in toregex(used))
         // - match . -> can rewrite s and replace the character at the position that matched with one of used.
         // This works for intersection, but not for semantic equality i think.
         // 


         // Create a map from not-char to replacement.
         

         if (tUsedSet.Contains(tMagicMarker))
            throw new Exception("magic marker is used");

         // Before we get out, rewrite .:
         tResult = tResult.Apply(r =>
         {
            if (r is CompleteRegex)
               return Choice(from c in tUsedSet select Letter(c), Letter(tMagicMarker));
            return r;
         });

         // Rewrite negations.
         tResult = tResult.Apply(r =>
         {
            if (r is NegatedLetterRegex)
               return Choice(from c in tUsedSet where c != ((NegatedLetterRegex)r).Letter select Letter(c), Letter(tMagicMarker));
            return r;
         });

         return tResult;
      }

      public enum RegexOptions
      {
         None = 0,
         IgnorePatternWhitespace = 1
      }

      internal abstract StringBuilder ToString(StringBuilder builder, Boolean asUnit);

      public override string ToString()
      {
         return ToString(new StringBuilder(), false).ToString();
      }

      #region Parser

      public static SimpleRegex Parse(String expression)
      {
         // todo: this should be ignore by default!
         return Parse(expression, RegexOptions.IgnorePatternWhitespace);
      }

      // todo: once my parsergenerator is working fully, use an older build to do this, ie self reference (!)
      // this may be possible if we don't need lookahead...
      /* BNF we're parsing by hand:
       *   regex  ::= union | simpleRegex
       *   union ::= regex '|' simpleRegex
       *   simpleRegex ::= concatRegex | basicRegex
       *   concatRegex ::= simpleRegex basicRegex
       *   basicRegex ::= notRegex modifier?
       *   modifier ::= '?' | '+' | '*' | '{' digit ',' [digit] '}'
       *   notRegex ::= '~'? elemRegex
       *   elemRegex ::= group | any | char | set
       *   group ::= '(' regex ')'
       *   any ::= '.'
       *   char ::= <any unicode char except used for grammar>
       *   set ::= '[' '^'? range | preSet ']'
       *   range ::= char '-' char
       *   preSet ::= char+
       *   
       * Rewritten as:
       *   regex ::= simpleRegex [ '|' simpleRegex ]
       *   simpleRegex ::= basicRegex+
       *   ...
       *   
       * We'll use translations:
       * A+ -> A A*
       * A{n,} -> A A ... A A*
       * A{n,m} -> A(n) | A(n+1) | ...
       * A? -> empty | A
       * 
       *   */
      public static SimpleRegex Parse(String regexExpr, RegexOptions options)
      {
         Int32 tPos = 0;
         try
         {
            var tContext = new RegexParseContext()
            {
               Position = 0, Expression = regexExpr, Options = options
            }; // (regexExpr, ref tPos);


            SimpleRegex tResult = Parse_Regex(tContext);

            if (tContext.Position != tContext.Expression.Length)
               throw new Exception("expected end of input");

            return tResult;
         }
         catch (Exception e)
         {
            throw new Exception("Parse error for regex '" + regexExpr + "' around position " + tPos + ": " + e.Message, e);
         }
      }

      public class RegexParseContext
      {
         public Int32 Position;
         public String Expression;
         public RegexOptions Options;

         public Char CurrentChar { get { return Expression[Position]; } }
         public Boolean HasMore { get { return Position < Expression.Length; } }

         public void MoveRight()
         {
            Position += 1;
         }
      }

      protected static void SkipWhitespace(RegexParseContext ctx) //(String expr, RegexOptions options, ref Int32 pos)
      {
         if ((ctx.Options & RegexOptions.IgnorePatternWhitespace) > 0)
         {
            for (; ctx.Position < ctx.Expression.Length && IsWhitespace(ctx.Expression[ctx.Position]); ctx.Position++)
            { }
         }
      }

      // todo: this must be done properly, we must match .net exactly
      // todo: we must think about \n special casing in general
      protected static Boolean IsWhitespace(Char c)
      {
         return Char.IsWhiteSpace(c);
      }

      protected static SimpleRegex Parse_Regex(RegexParseContext ctx) //(String expr, ref Int32 pos)
      {
         SimpleRegex left = Parse_SimpleRegex(ctx); //, ref pos);
         
         SkipWhitespace(ctx);
         
         if (ctx.HasMore && ctx.CurrentChar == '|')
         {
            ctx.Position += 1;

            return Choice(left, Parse_Regex(ctx)); //(expr, ref pos));
         }
         else
            return left;
      }

      protected static SimpleRegex Parse_SimpleRegex(RegexParseContext ctx) // (String expr, ref Int32 pos)
      {
         SimpleRegex left = Parse_BasicRegex(ctx); //(expr, ref pos);

         SkipWhitespace(ctx);

         if (ctx.HasMore && ctx.CurrentChar != ')' && ctx.CurrentChar != '|') // lookahead for ) and |
            return Sequence(left, Parse_SimpleRegex(ctx)); //(expr, ref pos));
         else
            return left;
      }

      protected static SimpleRegex Parse_BasicRegex(RegexParseContext ctx) //(String expr, ref Int32 pos)
      {
         SimpleRegex tNotRegex = Parse_NotRegex(ctx); //(expr, ref pos);
         //if (pos < expr.Length)

         SkipWhitespace(ctx);

         if (ctx.HasMore)
         {
            if (ctx.CurrentChar == '+')
            {
               ctx.Position += 1;
               return Sequence(tNotRegex, Star(tNotRegex));
            }
            else if (ctx.CurrentChar == '?')
            {
               ctx.Position += 1;
               return Choice(Empty, tNotRegex);
            }
            else if (ctx.CurrentChar == '*')
            {
               ctx.Position += 1;
               return Star(tNotRegex);
            }
            else if (ctx.CurrentChar == '{')
            {
               throw new NotSupportedException("todo: numbered ranges");
               //pos += 1;
               //String tIntStr = "";
               //for(Int32 i = pos; i < expr.Length && Char.IsNumber(expr[i]) ; i++) 
               //   tIntStr += expr[i];

            }
         }

         return tNotRegex;
      }

      protected static SimpleRegex Parse_NotRegex(RegexParseContext ctx) //(String expr, ref Int32 pos)
      {
         Boolean tNegate = false;

         SkipWhitespace(ctx);

         if (ctx.CurrentChar == '~')
         {
            tNegate = true;
            ctx.Position += 1;
         }

         SimpleRegex tRegex = Parse_ElemRegex(ctx); //(expr, ref pos);

         return tNegate ? tRegex.Negate() : tRegex;
      }

      protected static SimpleRegex Parse_ElemRegex(RegexParseContext ctx) //(String expr, ref Int32 pos)
      {
         SkipWhitespace(ctx);

         if (ctx.CurrentChar == '(')
         {
            ctx.Position += 1;
            
            SimpleRegex tResult = Parse_Regex(ctx); //(expr, ref pos);
            
            SkipWhitespace(ctx);
            if (ctx.CurrentChar != ')')
               throw new Exception("expected ')' at position " + ctx.Position);

            ctx.Position += 1;
            return tResult;
         }
         else if (ctx.CurrentChar == '.')
         {
            ctx.Position += 1;
            return Complete;
         }
         else if (ctx.CurrentChar == '[')
            return Parse_Set(ctx); //expr, ref pos);
         else
         {
            // no escaping, for now.
            return Letter(ReadLetter(ctx)); //expr, ref pos));
         }
      }

      //protected static readonly Char[] RegexChars = new[] { '[', ']', '(', ')', '^', '.', '{', '}', '*', '+', '-', '?', '|', '&', '\\' };

      // obviously this thing aint readonly
      protected static readonly HashSet<Char> RegexChars = new HashSet<char>()
      {
         '[', ']', '(', ')', '^', '.', '{', '}', '*', '+', '-', '?', '|', '&', '\\', '$'
      };

      // Support escaping.
      protected static Char ReadLetter(RegexParseContext ctx) //String expr, ref Int32 pos)
      {
         Char tResult;

         if (ctx.CurrentChar == '\\')
         {
            ctx.Position += 1;

            if (!RegexChars.Contains(ctx.CurrentChar))
               throw new Exception(String.Format("Invalid escaped character: '{0}'.", ctx.CurrentChar));

            tResult = ctx.CurrentChar;
            ctx.Position += 1;
            //return expr[pos++];
         }
         else
         {
            tResult = ValidateLetter(ctx.CurrentChar);
            ctx.Position += 1;
            //return ValidateLetter(ctx.CurrentChar);
            //return ValidateLetter(expr[pos++]);
         }

         return tResult;
      }

      protected static Char ValidateLetter(Char c)
      {
         // todo: do this properly
         //HashSet<Char> tSyntaxSet = new HashSet<char>();
         //tSyntaxSet.AddRange(RegexChars);
         //if (tSyntaxSet.Contains(c))
         if (RegexChars.Contains(c))
            throw new Exception(String.Format("Letter '{0}' is part of regex syntax.", c.ToString()));
         return c;
      }

      public static String Escape(String pString)
      {
         if (String.IsNullOrEmpty(pString))
            return pString;

         StringBuilder tResult = new StringBuilder((Int32)(pString.Length * 1.1)); // todo: good heuristic?
         for (Int32 i = 0; i < pString.Length; i++)
         {
            Char tChar = pString[i];

            if (RegexChars.Contains(tChar))
               tResult.Append('\\');
            
            tResult.Append(tChar);
         }
         return tResult.ToString();
      }

      public static String Unescape(String pString)
      {
         if (String.IsNullOrEmpty(pString))
            return pString;

         StringBuilder tResult = new StringBuilder(pString.Length); // less?

         for (Int32 i = 0; i < pString.Length; i++)
         {
            Char tChar = pString[i];

            if (tChar == '\\')
            {
               i += 1;

               if (i == pString.Length)
                  throw new Exception("Escape character \\ at end of string.");

               tChar = pString[i];

               if (!RegexChars.Contains(tChar))
                  throw new Exception(String.Format("Character '{0}' cannot be escaped.", pString[i]));

               tResult.Append(tChar);
            }
            else
            {
               if (RegexChars.Contains(tChar))
                  throw new Exception(String.Format("Character '{0}' must be escaped.", pString[i]));

               tResult.Append(tChar);
            }
         }

         return tResult.ToString();
      }

      // todo: support for (ignoring) non-capturing groups

      // Note: whitespace is NOT skipped within character classes/sets!
      protected static SimpleRegex Parse_Set(RegexParseContext ctx) //(String expr, ref Int32 pos)
      {
         Debug.Assert(ctx.CurrentChar == '['); // we tested this previously
         ctx.Position += 1; // skip [

         Boolean tNegate = false;
         if (ctx.CurrentChar == '^')
         {
            tNegate = true;
            ctx.Position += 1;
         }
         SimpleRegex tResult;
         List<Char> tCharList = new List<char>();
         tCharList.Add(ReadLetter(ctx)); //expr, ref pos));
         if (ctx.CurrentChar == '-')
         {
            ctx.Position += 1; // skip -
            Char tHi = ReadLetter(ctx); //(expr, ref pos);
            ctx.Position += 1; // skip ]
            if (tHi < tCharList[0])
               throw new Exception(String.Format("Invalid range: {0} > {1}.", tCharList[0], tHi));

            tResult = tNegate ? Range(tCharList[0], tHi).Invert() : Range(tCharList[0], tHi);
         }
         else
         {
            for (; ctx.CurrentChar != ']'; )
               tCharList.Add(ReadLetter(ctx)); //(expr, ref pos));
            
            ctx.Position += 1; // skip ']'

            if (tNegate) // it's none of the letters here
               tResult = And(from n in tCharList select new NegatedLetterRegex { Letter = n });
            else // it's a choice of any
               tResult = Choice(from n in tCharList select Letter(n));
         }

         return tResult; // note: Negate() is too strong!
         //return tNegate ? tResult.Negate() : tResult;
      }

      #endregion
   }

   public class VarRegex : SimpleRegex
   {
      public Int32 Value;

      // Clearly, if this is x, we're solving A ^ B = A ^ B (=x) with solution empty.
      // Otherwise, consider the variable (y, say) any other regex (that it represents). Clearly we assume y does not depend
      // on x so we can just return it as any regex.
      //
      // Note: left(x) = empty, nothing else matches up to x.
      //   right(x) = zero as there is nothing else that we can match if not x.
      //
      // Example: solve A x. This has no solutions as the only string that matches is an infinite string of As. This is solved by returning our Zero right.
      // Example (2): solve A x b: no solutions.
      // However, consider A x b | c, a solution is A* c b. BUT our conversion never presents a situation with a sequence of this type.
      // Ie, a variable (see variable convertinternal) always has a variable on the right, and sequence conversions either start with x in its right part, or
      // convert to sequences that have variables in their right parts, never their lefts.
      internal override Pair ConvertInternal(int x)
      {
         if (x == Value)
            return Pair(Empty, Zero); // Empty); // remove
         else
            return Pair(Empty, this);  // keep it
      }

      internal override bool Contains(int x)
      {
         return x == Value;
      }

      public override SimpleRegex Clone()
      {
         return new VarRegex { Value = this.Value };
      }

      internal override HashSet<SimpleRegex> PartialDeriv(char c)
      {
         throw new InvalidOperationException();
      }

      internal override bool IsEmpty()
      {
         throw new InvalidOperationException();
      }

      internal override bool IsZero()
      {
         throw new NotImplementedException();
      }

      internal override char[] Sigma()
      {
         throw new NotImplementedException();
      }

      internal override StringBuilder ToString(StringBuilder builder, Boolean asUnit)
      {
         // Ignore asUnit: this is always a unit, no parentheses needed.
         return builder.Append("Var(").Append(Value).Append(')');
      }

      public override SimpleRegex Apply(Func<SimpleRegex, SimpleRegex> f)
      {
         return f(this);
      }

      public override bool Equals(object obj)
      {
         VarRegex tOther = obj as VarRegex;
         return tOther != null && Value == tOther.Value;
      }

      public override int GetHashCode()
      {
         return Value;
      }

      /// <summary>
      /// Placeholder regex ("variable"), negating makes no sense and is an invalid operation were it to occur.
      /// </summary>
      /// <returns></returns>
      internal override SimpleRegex Negate()
      {
         throw new InvalidOperationException();
      }

      public override T Accept<T>(IRegexVisitor<T> visitor)
      {
         return visitor.Visit(this);
      }
   }

   public class AndRegex : SimpleRegex
   {
      public SimpleRegex Left, Right;

      public override SimpleRegex Apply(Func<SimpleRegex, SimpleRegex> f)
      {
         Left = Left.Apply(f);
         Right = Right.Apply(f);
         return f(this);
      }

      public override SimpleRegex Clone()
      {
         return new AndRegex { Left = this.Left.Clone(), Right = this.Right.Clone() };
      }

      internal override bool Contains(int x)
      {
         return Left.Contains(x) || Right.Contains(x);
      }

      // Must solve expression of the form A ^ B = L(x) & R(x), for some L and R depending on x.
      // It is quite clear (see Choice, Seq) that the solution is:
      //
      // (left(L) & left(R))+ (right(L) & right(R))
      //
      // Ie we first match up till x (which thus recursively continues forever) and finally match the rest.
      // N.B. (todo): same empty issue (due to + above) as for sequence.
      internal override Pair ConvertInternal(int x)
      {
         //return Pair(Empty, this); // todo: must figure out what convert does, is this correct?
         Pair tLeft = Left.ConvertInternal(x);
         Pair tRight = Right.ConvertInternal(x);
         return Pair(And(tLeft.Left, tRight.Right), And(tLeft.Right, tRight.Right));
      }

      public override bool Equals(object obj)
      {
         AndRegex tOther = obj as AndRegex;
         return tOther != null && tOther.Left.Equals(this.Left) && tOther.Right.Equals(this.Right);
      }

      public override int GetHashCode()
      {
         return ~(Left.GetHashCode() ^ Right.GetHashCode());
      }

      internal override bool IsEmpty()
      {
         return Left.IsEmpty() && Right.IsEmpty();
      }

      internal override bool IsZero()
      {
         return Left.IsZero() || Right.IsZero();
      }

      // not (A & B) -> not(A) | not(B)
      internal override SimpleRegex Negate()
      {
         return Choice(Left.Negate(), Right.Negate());
      }

      // returns p(left, c) INTERSECT p(right, c)
      // Any regex A can be written A = S_c(c S(p_c(A))) where S_c = sum (choice) over c and p_c is partial derivative wrt c.
      // Thus, given A and B, we can write A and B as such. For any input string s matching A and B (thus A & B), we must have that
      // there is a regex in p(A) and p(B) that matches s. Thus we can write
      // A & B = S_c(c S(p_c(A) & p_c(B))) where p_c(A) & p_c(B) = all in left set anded with all in right set. Clearly the same holds vice versa.
      internal override HashSet<SimpleRegex> PartialDeriv(char c)
      {
         var tLeftPartial = Left.PartialDeriv(c);
         var tRightPartial = Right.PartialDeriv(c);

         HashSet<SimpleRegex> tResult = new HashSet<SimpleRegex>();
         foreach(SimpleRegex tLeftRegex in tLeftPartial)
            foreach (SimpleRegex tRightRegex in tRightPartial)
            {
               tResult.Add(And(tLeftRegex.Clone(), tRightRegex));
               //tResult.Add(tLeftRegex.Intersect(tRightRegex));
            }

         return tResult;

         //HashSet<SimpleRegex> tResult = Left.PartialDeriv(c);
         //tResult.IntersectWith(Right.PartialDeriv(c));

         //// debug
         //if (Left.PartialDeriv(c).Count == 1 && Right.PartialDeriv(c).Count == 1)
         //{
         //   SimpleRegex tIntersection = Left.PartialDeriv(c).First().Intersect(new Dictionary<Pair, SimpleRegex>(), 0, Right.PartialDeriv(c).First());
         //}

         //return tResult;
      }

      internal override char[] Sigma()
      {
         return Left.Sigma().Merge(Right.Sigma());
      }

      internal override StringBuilder ToString(StringBuilder builder, Boolean asUnit)
      {
         if (asUnit)
            builder.Append('(');

         Left.ToString(builder, !(Left is AndRegex));

         builder.Append(" & ");

         Right.ToString(builder, !(Right is AndRegex));

         if (asUnit)
            builder.Append(')');

         return builder;

         //if (Left is ChoiceRegex)
         //{
         //   builder.Append('(');
         //   Left.ToString(builder);
         //   builder.Append(')');
         //}
         //else
         //   Left.ToString(builder);

         //builder.Append(" & ");

         //if (Right is ChoiceRegex)
         //{
         //   builder.Append('(');
         //   Right.ToString(builder);
         //   builder.Append(')');
         //}
         //else
         //   Right.ToString(builder);

         //return builder;
      }

      public override T Accept<T>(IRegexVisitor<T> visitor)
      {
         return visitor.Visit(this);
      }
   }

   public class ChoiceRegex : SimpleRegex
   {
      public SimpleRegex Left, Right;

      internal override bool Contains(int x)
      {
         return Left.Contains(x) || Right.Contains(x);
      }

      public override SimpleRegex Clone()
      {
         return new ChoiceRegex() { Left = this.Left.Clone(), Right = this.Right.Clone() };
      }

      internal override HashSet<SimpleRegex> PartialDeriv(Char c)
      {
         return Left.PartialDeriv(c).Merge(Right.PartialDeriv(c));
         //var tLeftDerivs = Left.PartialDeriv(c);
         //var tRightDerivs = Right.PartialDeriv(c);
         //tLeftDerivs.AddRange(tRightDerivs);
         //return tLeftDerivs.Nub();
      }

      // Converts left, converts right (left' and right') then returns:
      //
      // { left'.left | right'.left , left'.right | right'.right }
      //
      // If neither left nor right contains x, left' = { empty, left }
      // and right' = { empty, right }.
      // Then Choice(left'.left, right'.left) = Choice(empty, empty) = empty.
      // Then Choice(left'.right, right'.right) = Choice(left, right) = this.
      // => we're returning { empty, this }
      // 
      // A ^ B = a x | b R = a (A ^ B) | b R, so input either matches b R or a b R or a a b R or a+ bR =>
      // x = A ^ B = a* b R. WLOG: any R1 instead of a (not containing x).
      // ie A ^ B = R1 x | R2 => A ^ B = R1* R2
      // Similarly, we see that R1 x | R2 x | R => (R1 | R2)* R. This is either direct, or R1 x | R2 x = (R1 | R2) x etc.
      //
      // Generalising further, we wish to handle R1 | R(x), which is the general choice case.
      // Here R(x) denotes some regular expression with a dependency on x.
      // Following the above logic, it is clear that
      //
      // A ^ B = left(R)* (R1 | right(R))
      //
      // leaving out the empty left(R1) = empty as it has no dependency on x.
      // 
      // So left(R)  := regex that matches before A ^ B. 
      //                More formal: regex f is in left(R) iff any input a matches f then if b matches x, ab matches R(x).
      //                In terms of a DAG: left(R) is the DAG before the state x, which is "return to start".
      //    right(R) := matches not x or anything after A ^ B. More formal: regex f is in right(R) iff a matches f then ??
      //                Define recursively?
      //                In DAG terms: this is the DAG that does not include x in its path.
      // For more on this: suppose we have some string s we're matching. Either we match right(R), or we match left(R) then x, and then repeat until s is matched.
      //
      // Problem: Ax | Zero, clearly this should solve to A+, but solves to A* (empty | zero).
      // 
      // A* (empty | zero)
      //
      // ax | zero | b -> ax | b -> a*b
      //
      // Solution is that of Ax.
      //
      //
      //
      // TODO (solves empty issue): instead of doing A*B, we should let convert do A+B, then choice should return (A|empty)+B = A*B.
      internal override Pair ConvertInternal(int x)
      {
         // todo: if neither left nor right contains x, we should return empty instead of choice(empty, empty).
         Pair tConvLeft = Left.ConvertInternal(x);
         Pair tConvRight = Right.ConvertInternal(x);
         return Pair(Choice(tConvLeft.Left, tConvRight.Left), Choice(tConvLeft.Right, tConvRight.Right));
      }

      internal override char[] Sigma()
      {
         return Left.Sigma().Merge(Right.Sigma()).Nub(); // todo: can do merge/nub in one go
      }

      internal override bool IsZero()
      {
         return Left.IsZero() && Right.IsZero();
      }

      internal override bool IsEmpty()
      {
         return Left.IsEmpty() || Right.IsEmpty();
      }

      internal override StringBuilder ToString(StringBuilder builder, Boolean asUnit)
      {
         if (asUnit)
            builder.Append('(');

         Left.ToString(builder, !(Left is ChoiceRegex));

         builder.Append(" | ");

         Right.ToString(builder, !(Right is ChoiceRegex));

         if (asUnit)
            builder.Append(')');

         return builder;
      }

      public override bool Equals(object obj)
      {
         ChoiceRegex tOther = obj as ChoiceRegex;
         return tOther != null && tOther.Left.Equals(Left) && tOther.Right.Equals(Right);
      }

      public override int GetHashCode()
      {
         return Left.GetHashCode() ^ Right.GetHashCode();
      }

      public override SimpleRegex Apply(Func<SimpleRegex, SimpleRegex> f)
      {
         Left = Left.Apply(f);
         Right = Right.Apply(f);
         return f(this);
      }

      // not(A | B) -> not(A) & not(B)
      // verify: not(not(A) & not(B)) = not(not(A)) | not(not(B))
      internal override SimpleRegex Negate()
      {
         return And(Left.Negate(), Right.Negate());
      }

      public override T Accept<T>(IRegexVisitor<T> visitor)
      {
         return visitor.Visit(this);
      }
   }

   // todo: unify letterregx, negatedletter and rangeregx into a single UnitRegex:
   // class UnitRegex, this has a negated property making NegatedLetter no longer needed
   
   public class LetterRegex : SimpleRegex
   {
      public Char Letter;

      internal override HashSet<SimpleRegex> PartialDeriv(Char c)
      {
         if (c == Letter)
            return new HashSet<SimpleRegex>() { SimpleRegex.Empty };
         else
            return new HashSet<SimpleRegex>(); // todo: can we use a single instance?
      }

      public override SimpleRegex Clone()
      {
         return new LetterRegex() { Letter = this.Letter };
      }

      internal override bool Contains(int x)
      {
         return false;
      }

      // Does not contain the variable, so return { empty, this }.
      internal override Pair ConvertInternal(int x)
      {
         return Pair(Empty, this);
      }

      internal override StringBuilder ToString(StringBuilder builder, bool asUnit)
      {
         return builder.Append(Letter);
      }

      internal override bool IsEmpty()
      {
         return false;
      }

      internal override bool IsZero()
      {
         return false;
      }

      internal override char[] Sigma()
      {
         return new Char[] { Letter };
      }

      public override bool Equals(object obj)
      {
         LetterRegex tOther = obj as LetterRegex;
         return tOther != null && tOther.Letter == Letter;
      }

      public override int GetHashCode()
      {
         return Letter.GetHashCode();
      }

      public override SimpleRegex Apply(Func<SimpleRegex, SimpleRegex> f)
      {
         return f(this);
      }

      // returns not(a)
      // not(a) = n(a) | e
      // not(n(a) | e) = a & not(e) = a
      // Update: let not(a) denote all characters not matching a, with ~a = not(a) | empty.
      // We then have:
      // ~a = empty | not(a) .* | a .+
      internal override SimpleRegex Negate()
      {
         //return Choice(Empty, new NegatedLetterRegex() { Letter = this.Letter });
         return Choice(Empty, Choice(Sequence(new NegatedLetterRegex { Letter = this.Letter }, Star(Complete)), Sequence(this.Clone(), Sequence(Complete, Star(Complete)))));
      }

      public override T Accept<T>(IRegexVisitor<T> visitor)
      {
         return visitor.Visit(this);
      }
   }

   // won't work because of unwieldy sigma -> instead we'll rewrite to "unique symbols"
   // Represents all letters that are not the given letter. NOTE: this is different from ~a!
   public class NegatedLetterRegex : SimpleRegex
   {
      public Char Letter;

      // not(a) = b | c | .. all letters except a.
      // p(not(a)) = p(b | c | ...) = p(b) | p(c) | ... 
      //   = empty set if x = a
      //   = p(x) = empty
      internal override HashSet<SimpleRegex> PartialDeriv(char c)
      {
         if (c == Letter)
            return new HashSet<SimpleRegex>(); // empty set
         else
            return new HashSet<SimpleRegex>() { Empty };
      }

      public override SimpleRegex Clone()
      {
         return new NegatedLetterRegex() { Letter = this.Letter };
      }

      internal override bool Contains(int x)
      {
         return false;
      }

      // Does not contain letter, so returns { empty, this }.
      internal override Pair ConvertInternal(int x)
      {
         return Pair(Empty, this);
      }

      public override bool Equals(object obj)
      {
         NegatedLetterRegex tOther = obj as NegatedLetterRegex;
         return tOther != null && tOther.Letter == this.Letter;
      }

      public override int GetHashCode()
      {
         return ~Letter.GetHashCode(); // opposite of what we get for LetterRegex
      }

      internal override bool IsEmpty()
      {
         return false;
      }

      internal override bool IsZero()
      {
         return false;
      }

      internal override char[] Sigma()
      {
         // NOTE: this class IS intended for rewrite.
         return new Char[] { Letter };
      }

      internal override StringBuilder ToString(StringBuilder builder, bool asUnit)
      {
         return builder.Append('~').Append(Letter);
      }

      public override SimpleRegex Apply(Func<SimpleRegex, SimpleRegex> f)
      {
         return f(this);
      }

      // not(not(a)) -> a
      // Update:
      // Negated letter can be seen as any other letter, so it follows the same formula.
      // Otherwise, we can see this by deriving ~not(a) = ~(b|c) = (empty|not(b).*|b.+)&(empty|not(c).*|c.+) = empty | a.* | b.+ | c.+ = empty | a.* | not(a).+
      internal override SimpleRegex Negate()
      {
         //return Letter(this.Letter);
         return Choice(Empty, Choice(Sequence(Letter(this.Letter), Star(Complete)), Sequence(this.Clone(), Sequence(Complete, Star(Complete)))));
      }

      public override T Accept<T>(IRegexVisitor<T> visitor)
      {
         return visitor.Visit(this);
      }
   }

   public class SeqRegex : SimpleRegex
   {
      public SimpleRegex Left, Right;

      public override SimpleRegex Clone()
      {
         return new SeqRegex() { Left = this.Left.Clone(), Right = this.Right.Clone() };
      }

      internal override HashSet<SimpleRegex> PartialDeriv(char c)
      {
         if (Left.IsEmpty())
         {
            var s1 = new HashSet<SimpleRegex>();
            foreach (SimpleRegex tPart in Left.PartialDeriv(c))
               s1.Add(new SeqRegex() { Left = tPart, Right = this.Right });

            return s1.Merge(Right.PartialDeriv(c));
         }
         else
         {
            HashSet<SimpleRegex> tResult = new HashSet<SimpleRegex>();
            foreach (SimpleRegex tPart in Left.PartialDeriv(c))
               tResult.Add(new SeqRegex() { Left = tPart, Right = this.Right });
            return tResult;
         }
      }

      internal override bool Contains(int x)
      {
         return Left.Contains(x) || Right.Contains(x);
      }

      // Again, solving A ^ B = L(x) R(x) where L and R are two regular expressions dependent on x.
      // Due to the way, however, that intersect constructs sequences, L(x) = L: we don't need to consider the L(x) case.
      // Thus clearly, if right does not contain the variable x, we can just return { empty, this }: no changes.
      // Consider A ^ B = L a (A ^ B) = L a x. Clearly x = (L a)+.
      // In general A ^ B = L R(x) => x = (L left(R))+ right(R). (left(R) is match up to x, right(R) is match after x).
      // Note: A x has no solutions (but infinite strings of a). However, this sequence should be considered part of (!) a choice which will allow
      // for solutions. In the general case R(x) here, of course, it can be that that contains a non-zero path, so that this provides a solution.
      internal override Pair ConvertInternal(int x)
      {
         Debug.Assert(!Left.Contains(x), "left should never contain a variable in a sequence");

         if (Right.Contains(x))
         {
            Pair tRightPair = Right.ConvertInternal(x);
            return Pair(new SeqRegex() { Left = this.Left, Right = tRightPair.Left }, tRightPair.Right);
         }
         else
            return Pair(Empty, this);
      }

      internal override bool IsEmpty()
      {
         return Left.IsEmpty() && Right.IsEmpty();
      }

      internal override bool IsZero()
      {
         return Left.IsZero() || Right.IsZero();
      }

      internal override StringBuilder ToString(StringBuilder builder, bool asUnit)
      {
         if (asUnit)
            builder.Append('(');

         Left.ToString(builder, !(Left is SeqRegex));

         builder.Append(' ');

         Right.ToString(builder, !(Right is SeqRegex));

         if (asUnit)
            builder.Append(')');

         return builder;
      }

      internal override char[] Sigma()
      {
         return Left.Sigma().Merge(Right.Sigma()).Nub();  
      }

      public override bool Equals(object obj)
      {
         SeqRegex tOther = obj as SeqRegex;
         return tOther != null && tOther.Left.Equals(Left) && tOther.Right.Equals(Right);
      }

      public override int GetHashCode()
      {
         return Left.GetHashCode() ^ Right.GetHashCode();
      }

      public override SimpleRegex Apply(Func<SimpleRegex, SimpleRegex> f)
      {
         Left = Left.Apply(f);
         Right = Right.Apply(f);
         return f(this);
      }

      // todo: go over commenst and clean them up!!!

      // not (A.B) -> not(A) | A.not(B)
      // verify: not(not(A) | A.not(B)) = not(not(A)) & not(A.not(B))
      //    = not(not(A)) & (not(A) | A.not(not(B)))
      //    = A & (not(A) | A.B)                                          (line added later:)  = (A & not(A)) | (A & (A.B)) = 0 | A.B = A.B
      //    = A & (A.B) = A.B
      // Update: we must guard against the option not(A) matching AB (but not A).
      // 1. Performs a rewrite to a sequence of the form a B (or a regex containing such sequences).
      // 2. Negate a B using: ~(a B) = empty | not(a) .* | a ~B. todo: write down verification here
      // todo: if a sequence is rewritten, it may again be rewritten once negation is called (recursively). E.g. it's
      //   rewritten to A | B where A = a B. When A.Negate is called it is again rewritten, unnecessarily. So, we should do the rewrite
      //   elsehere for efficiency.
      // todo: cloning and negating, same thing? Belowe we use clone but not where negating.. check
      internal override SimpleRegex Negate()
      {
         SimpleRegex tSelfRewritten = this.Accept(new NegationRewriter());

         if (tSelfRewritten is SeqRegex)
         {
            SeqRegex tSequence = (SeqRegex)tSelfRewritten;
            SimpleRegex tLeftNegation;
            if (tSequence.Left is LetterRegex)
            {
               tLeftNegation = new NegatedLetterRegex { Letter = ((LetterRegex)tSequence.Left).Letter };
            }
            else if (tSequence.Left is NegatedLetterRegex)
            {
               tLeftNegation = new LetterRegex { Letter = ((NegatedLetterRegex)tSequence.Left).Letter };
            }
            else if (tSequence.Left is RangeRegex)
            {
               tLeftNegation = tSequence.Left;
               ((RangeRegex)tLeftNegation).Negated = !((RangeRegex)tLeftNegation).Negated;
            }
            else if (tSequence.Left == Complete)
            {
               tLeftNegation = Zero;
            }
            else
               throw new InvalidOperationException("Bad rewrite");

            return Choice(new[] { Empty, Sequence(tLeftNegation, Star(Complete)), Sequence(tSequence.Left, tSequence.Right.Negate()) });
         }
         else
            return tSelfRewritten.Negate();

         //return Choice(Left.Negate(), Sequence(Left.Clone(), Right.Negate()));
         //return Choice(And(Left.Negate(), Sequence(Left.Clone(), Right.Negate())), Sequence(Left.Clone(), Right.Negate()));
      }

      public override T Accept<T>(IRegexVisitor<T> visitor)
      {
         return visitor.Visit(this);
      }

      internal class NegationRewriter : SimpleVisitor // when this works, change to simplifying?
      {
         // Rewrite sequences such that are all of the form a B for some letter a and a regex B.
         public override SimpleRegex Visit(SeqRegex sequence)
         {
            // Rewrite the left part of this sequence.
            SimpleRegex tLeft = sequence.Left.Accept(this);

            if (tLeft == Empty)
               return sequence.Right;

            if (tLeft == Zero)
               return Zero; // done

            // (A | B) C = (A C) | (B C)
            if (tLeft is ChoiceRegex)
            {
               ChoiceRegex tChoice = (ChoiceRegex)tLeft;
               return Choice(Sequence(tChoice.Left, sequence.Right), Sequence(tChoice.Right, sequence.Right));
            }

            // (A & B) C = (A C) & (B C)
            if (tLeft is AndRegex)
            {
               AndRegex tAnd = (AndRegex)tLeft;
               return And(Sequence(tAnd.Left, sequence.Right), Sequence(tAnd.Right, sequence.Right));
            }

            // (A B) C = A (B C)
            if (tLeft is SeqRegex)
            {
               SeqRegex tSequence = (SeqRegex)tLeft;
               // Note: left has been rewritten, so tSequence.Left has been rewritten.
               Debug.Assert(!(tSequence.Left is SeqRegex));
               return Sequence(tSequence.Left, Sequence(tSequence.Right, sequence.Right));
            }

            if (tLeft is RangeRegex || tLeft is LetterRegex || tLeft is NegatedLetterRegex || tLeft is CompleteRegex)
            {
               sequence.Left = tLeft;
               return sequence;
            }

            throw new NotSupportedException("Regex type not supported " + tLeft.GetType());
         }
      }
   }

   public class KleeneRegex : SimpleRegex
   {
      public SimpleRegex Operand;

      public override SimpleRegex Apply(Func<SimpleRegex, SimpleRegex> f)
      {
         Operand = Operand.Apply(f);
         return f(this);
      }

      public override SimpleRegex Clone()
      {
         return new KleeneRegex() { Operand = this.Operand.Clone() };
      }

      internal override HashSet<SimpleRegex> PartialDeriv(char c)
      {
         HashSet<SimpleRegex> tDerivs = new HashSet<SimpleRegex>();
         foreach (SimpleRegex tPart in Operand.PartialDeriv(c))
            tDerivs.Add(new SeqRegex() { Left = tPart, Right = this });
         return tDerivs;
      }

      internal override bool Contains(int x)
      {
         return Operand.Contains(x);
      }

      // todo: NOT correct
      // Here we're solving A ^ B = R(x)*.
      // Clearly this is solved by
      //
      // left(R)* right(R)
      //
      // See Choice and others: match up to x, then enter x and continue. At some point this stops due to *, then finish off rest.
      internal override Pair ConvertInternal(int x)
      {
         //return Pair(Empty, this); // todo: im not entirely sure if correct, but i think it is
         Pair tConversion = Operand.ConvertInternal(x);
         return Pair(tConversion.Left, tConversion.Right);
      }

      internal override bool IsEmpty()
      {
         return true;
      }

      internal override bool IsZero()
      {
         return false;
      }

      internal override StringBuilder ToString(StringBuilder builder, bool asUnit)
      {
         return Operand.ToString(builder, asUnit).Append('*');
      }

      internal override char[] Sigma()
      {
         return Operand.Sigma();
      }

      public override bool Equals(object obj)
      {
         KleeneRegex tOther = obj as KleeneRegex;
         return tOther != null && tOther.Operand.Equals(this.Operand);
      }

      public override int GetHashCode()
      {
         return Operand.GetHashCode();
      }

      // A* = e | A | AA | AAA | ...
      // So not(A*) = not(e) & not(A) & not(AA) ...
      //  we get intuitively:
      // not(A*) = not(e) & not(A)*
      // verify:
      // not(not(A*)) = not(not(e) & not(A)*)
      //              = not(not(e)) | not(not(A)*)
      //              = e | (not(e) & not(not(A))*)
      //              = e | (not(e) & A*) = e | A* = A*
      internal override SimpleRegex Negate()
      {
         return And(Empty.Negate(), Star(Operand.Negate()));
      }

      public override T Accept<T>(IRegexVisitor<T> visitor)
      {
         return visitor.Visit(this);
      }
   }

   public class EmptyRegex : SimpleRegex
   {
      private static readonly EmptyRegex _sEmptyRegex = new EmptyRegex();

      protected EmptyRegex() {}

      static EmptyRegex() {}

      public static EmptyRegex Instance { get { return _sEmptyRegex; } }

      internal override HashSet<SimpleRegex> PartialDeriv(char c)
      {
         return new HashSet<SimpleRegex>();
      }

      public override SimpleRegex Clone()
      {
         return Empty;
      }

      // Does not contain x, so return { empty, empty } (self).
      internal override Pair ConvertInternal(int x)
      {
         return Pair(Empty, Empty);
      }

      internal override bool Contains(int x)
      {
         return false;
      }

      internal override bool IsEmpty()
      {
         return true;
      }

      internal override bool IsZero()
      {
         return false;
      }

      internal override StringBuilder ToString(StringBuilder builder, bool asUnit)
      {
         return builder.Append('ε');
      }

      internal override char[] Sigma()
      {
         return EmptyCharArray;
      }

      // we intend empty to be a singleton.. but we'll do this anyway
      public override bool Equals(object obj)
      {
         return obj is EmptyRegex;
      }

      public override int GetHashCode()
      {
         return Int32.MaxValue - 42;
      }

      public override SimpleRegex Apply(Func<SimpleRegex, SimpleRegex> f)
      {
         return f(this);
      }

      // not (e) = a | b | ... | zero, all letters plus zero
      // so not(not(e)) = not(a) & not(b) ... & not(zero) = e
      // Update
      // not(e) = . .*
      // Check: not(not(e)) = not(. .*) = not(.) | . not(.*) = e | . 0 = e
      internal override SimpleRegex Negate()
      {
         //return Choice(Complete, Zero);
         return Sequence(Complete, Star(Complete)); // todo: what about zero?
      }

      public override T Accept<T>(IRegexVisitor<T> visitor)
      {
         return visitor.Visit(this);
      }
   }

   // todo: matche everything (1 char at a time) ie = a | b | c ..., excluding empty.
   public class CompleteRegex : SimpleRegex 
   {
      private static readonly CompleteRegex _sInstance = new CompleteRegex();

      public static CompleteRegex Instance { get { return _sInstance; } }

      protected CompleteRegex()
      {
      }

      static CompleteRegex()
      {
      }

      public override SimpleRegex Apply(Func<SimpleRegex, SimpleRegex> f)
      {
         return f(this);
      }

      public override SimpleRegex Clone()
      {
         return Complete;
      }

      internal override bool Contains(int x)
      {
         return false;
      }

      // Not needed, as the Complete regex is a special node that should be written out.
      internal override Pair ConvertInternal(int x)
      {
         throw new InvalidOperationException("regex should have been rewritten");
      }

      public override bool Equals(object obj)
      {
         return obj is CompleteRegex; // todo: check singleton?
      }

      public override int GetHashCode()
      {
         return 234978;
      }

      internal override bool IsEmpty()
      {
         return false;
      }

      internal override bool IsZero()
      {
         return false;
      }

      // not(a|b|..|z) = not(a) & not(b) ... & not(z)
      //               = (n(a) & n(b) ... n(z) ) | Empty = Zero | Empty = Empty
      internal override SimpleRegex Negate()
      {
         return Empty; // todo: what about zero?
      }

      // As complete can be viewed a | b | c | ... | z, ie the whole "alphabet", we can 
      // get the partial deriv for 'c' as:
      // partialderiv(c | .) = partialderiv(c) union partialderiv( a | b | d ...)
      // = { empty } union {} = { empty }
      internal override HashSet<SimpleRegex> PartialDeriv(char c)
      {
         return new HashSet<SimpleRegex>() { Empty }; // todo: this is used in some other places, can we use a single instance?
      }

      // can't meaningfully implement, anyway, this class is intended for rewriting only.
      internal override char[] Sigma()
      {
         return EmptyCharArray;
      }

      internal override StringBuilder ToString(StringBuilder builder, bool asUnit)
      {
         return builder.Append('.');
      }

      public override T Accept<T>(IRegexVisitor<T> visitor)
      {
         return visitor.Visit(this);
      }
   }

   public class RangeRegex : SimpleRegex
   {
      public Boolean Negated = false;

      public Char Low, High; // todo: should use constructor and check Low <= High

      public override SimpleRegex Apply(Func<SimpleRegex, SimpleRegex> f)
      {
         return f(this);
      }

      public override SimpleRegex Clone()
      {
         return new RangeRegex() { Negated = this.Negated, Low = this.Low, High = this.High };
      }

      internal override bool Contains(int x)
      {
         return false;
      }

      public Boolean Contains(RangeRegex other)
      {
         return other.Low >= this.Low && other.High <= this.High;
      }

      public Boolean Contains(Char c)
      {
         return c >= this.Low && c <= this.High;
      }

      // Not implemented as this is a special regex node that must be rewritten.
      internal override Pair ConvertInternal(int x)
      {
         throw new InvalidOperationException("regex should have been rewritten");
      }

      public Int32 Length { get { return ((Int32)(High - Low) + 1); } }

      public override bool Equals(object obj)
      {
         RangeRegex tOther = obj as RangeRegex;
         return tOther != null && tOther.Low == this.Low && tOther.High == this.High && tOther.Negated == this.Negated;
      }

      public Boolean OverlapsWith(RangeRegex other)
      {
         return (Low >= other.Low && Low <= other.High) || (High <= other.High && High >= other.Low);
      }

      public override int GetHashCode()
      {
         return Low.GetHashCode() ^ High.GetHashCode() ^ Negated.GetHashCode();
      }

      internal override bool IsEmpty()
      {
         return false;
      }

      internal override bool IsZero()
      {
         return false;
      }

      /// <summary>
      /// Changes the range from a postive to negative range and vice versa. Eg [^a-z] -> [a-z]. 
      /// Returns self.
      /// </summary>
      internal virtual RangeRegex Invert()
      {
         Negated = !Negated;
         return this;
      }

      // Can not negate, so rewrite must tackle ranges first, and replace with marker.
      internal override SimpleRegex Negate()
      {
         RangeRegex tNegation = (RangeRegex)this.Clone();
         tNegation.Negated = !this.Negated;
         return Choice(Empty, Choice(Sequence(tNegation, Star(Complete)), Sequence(this.Clone(), Sequence(Complete, Star(Complete)))));
      

         // Old code:
         //RangeRegex tResult = (RangeRegex)this.Clone();
         //tResult.Negated = !this.Negated;
         //return tResult;
      }

      // note: range regular expressions are intended to be rewritten, so this needs no implementation.
      // If you get here, it indicates a bug.
      internal override HashSet<SimpleRegex> PartialDeriv(char c)
      {
         throw new NotImplementedException();
      }

      internal override char[] Sigma()
      {
         return new Char[] { Low, High };
      }

      internal override StringBuilder ToString(StringBuilder builder, bool asUnit)
      {
         builder.Append('[');

         if (Negated)
            builder.Append('^');

         return builder.Append(Low).Append('-').Append(High).Append(']');
      }

      public override T Accept<T>(IRegexVisitor<T> visitor)
      {
         return visitor.Visit(this);
      }
   }

   public class ZeroRegex : SimpleRegex
   {
      protected ZeroRegex() {}

      private readonly static ZeroRegex _sInstance = new ZeroRegex();

      static ZeroRegex() { }

      public static ZeroRegex Instance { get { return _sInstance; } }

      internal override HashSet<SimpleRegex> PartialDeriv(char c)
      {
         return new HashSet<SimpleRegex>();
      }

      public override SimpleRegex Clone()
      {
         return Zero;
      }

      // Does not contain x, so return { empty, self } = { empty, zero }.
      // Update: instead this should return { zero, zero }. Consider zero x | b, clearly the zero part we can't even enter as zero never matches.
      // Thus returning zero avoids that part altogether. zero x | b = b (as rewrite).
      internal override Pair ConvertInternal(int x)
      {
         //return Pair(Empty, Zero);
         return Pair(Zero, Zero);
      }

      internal override bool Contains(int x)
      {
         return false;
      }

      internal override bool IsEmpty()
      {
         return false;
      }

      internal override bool IsZero()
      {
         return true;
      }

      internal override char[] Sigma()
      {
         return EmptyCharArray;
      }

      internal override StringBuilder ToString(StringBuilder builder, bool asUnit)
      {
         return builder.Append('∅');
      }

      // again, really a singleton.. 
      public override bool Equals(object obj)
      {
         return obj is ZeroRegex;
      }

      public override int GetHashCode()
      {
         return 0;
      }

      public override SimpleRegex Apply(Func<SimpleRegex, SimpleRegex> f)
      {
         return f(this);
      }

      public override T Accept<T>(IRegexVisitor<T> visitor)
      {
         return visitor.Visit(this);
      }

      // not(0) = a | b | .. | empty
      // not(not(0)) = not(a) & not(b) & ... & not(empty) = 0
      // 
      internal override SimpleRegex Negate()
      {
         return Star(Complete);
         //return Choice(Complete, Empty);
      }
   }

   //// todo
   //public class NegatedRegex : Regex
   //{
   //   public Regex Operand;
   //}
}
