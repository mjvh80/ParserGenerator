using System;
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
      public static SimpleRegexBuilder Default = new SimpleRegexBuilder();

      internal HashSet<Char> mAlphabet;
      internal HashSet<RangeRegex> mRangeSet;

      protected Dictionary<RangeRegex, Char> mRangeMarkerMap = new Dictionary<RangeRegex, char>();
      protected readonly Char mMagicMarker = '\uFADE'; // todo: must choose something dynamically
         

      public SimpleRegexBuilder() : this(null, null) { }

      public SimpleRegexBuilder(HashSet<Char> alphabet, HashSet<RangeRegex> rangeSet) // todo: i think another type for ranges here may be better
      {
         mAlphabet = alphabet ?? new HashSet<Char>(); // todo: copy instead?

#if DEBUG
         if (rangeSet != null)
            foreach (RangeRegex tRange in rangeSet)
               if (!alphabet.Contains(tRange.Low) || !alphabet.Contains(tRange.High))
                  throw new InvalidOperationException("invalid range: boundaries not included in alphabet");

         if (mAlphabet.Contains(mMagicMarker)) // todo: must ensure this never happens, this is a safety catch for now
            throw new Exception("magic marker is used");
#endif

         mRangeSet = CreateDisjointRangeSet(rangeSet);

         // Update the alphabet.
         foreach (RangeRegex tRange in mRangeSet)
         {
            alphabet.Add(tRange.Low);
            alphabet.Add(tRange.High);
         }
         
         // Build marker map.
         Char tMarkerStart = 'A';// '\uf000'; // say, must find something "proper" todo
         foreach (RangeRegex tDisjointRange in mRangeSet)
         {
            if (tMarkerStart == '\uffff')
               throw new Exception("Markers exhausted"); // this is bad, but itll have to do for now..

            mRangeMarkerMap.Add(tDisjointRange, tMarkerStart += '\x0001');
         }
      }

      protected static HashSet<RangeRegex> CreateDisjointRangeSet(IEnumerable<RangeRegex> rangeSet)
      {
         HashSet<RangeRegex> tDisjointSet = new HashSet<RangeRegex>();
         if (rangeSet != null)
            foreach (RangeRegex tRange in rangeSet)
            {
               if (tRange.Negated)
                  throw new InvalidOperationException("range must not be negated"); // simply do here?
               SimpleRegex.AddRangeDisjoint(tRange, tDisjointSet); // todo: put this method somewhere else?
            }
         return tDisjointSet;
      }

      public SimpleRegex Parse(String regexExpr)
      {
         SimpleRegex tResult = SimpleRegex.Parse(regexExpr);
         return Normalize(tResult);
      }

      protected SimpleRegex Normalize(SimpleRegex targetRegex)
      {
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
         tResult = tResult.Apply(r =>
         {
            if (r is RangeRegex)
            {
               RangeRegex tRange = (RangeRegex)r;
               var tSplitRanges = SimpleRegex.SplitOnLetters(tRange, mAlphabet);
               SimpleRegex tRewrite = SimpleRegex.Choice(from tSplitRange in tSplitRanges
                                             select tSplitRange.Length == 1 ?
                                                (SimpleRegex)Letter(tSplitRange.Low) : tSplitRange);

               return tRange.Negated ? tRewrite.Negate() : tRewrite;
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
         tResult = tResult.Apply(r =>
         {
            if (r is CompleteRegex)
               return SimpleRegex.Choice(from c in mAlphabet select Letter(c), Letter(mMagicMarker));
            return r;
         });

         // Rewrite negated letters.
         tResult = tResult.Apply(r =>
         {
            if (r is NegatedLetterRegex)
               return SimpleRegex.Choice(from c in mAlphabet where c != ((NegatedLetterRegex)r).Letter select Letter(c), Letter(mMagicMarker));
            return r;
         });

         return tResult;
      }



      // We'll only provide methods here that do *not* need additional rewriting.
      public static ChoiceRegex Choice(SimpleRegex left, SimpleRegex right) { return new ChoiceRegex() { Left = left, Right = right }; }
      public static KleeneRegex Star(SimpleRegex op) { return new KleeneRegex() { Operand = op }; }
      public static SeqRegex Sequence(SimpleRegex left, SimpleRegex right) { return new SeqRegex() { Left = left, Right = right }; }
     
      public static LetterRegex Letter(Char c) { return new LetterRegex() { Letter = c }; }

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
      internal static readonly SimpleRegex Empty = new EmptyRegex();

      ///<summary>Matches nothing.</summary>
      internal static readonly SimpleRegex Zero = new ZeroRegex();

      /// <summary>
      /// Matches everything (.).
      /// </summary>
      internal static readonly SimpleRegex Complete = new CompleteRegex();

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

      /// <summary>
      /// Determines whether the given regexes intersect, internally performs a rewrite.
      /// </summary>
      // todo: should just be called Intersect
      public static Boolean RewritesIntersect(SimpleRegex left, SimpleRegex right)
      {
         SimpleRegex.CloneAndRewrite(ref left, ref right);
         return left.Intersects(right);
      }

      /// <summary>
      /// Determines whether given regexes are equal (rewrites internally).
      /// </summary>
      /// <param name="left"></param>
      /// <param name="right"></param>
      /// <returns></returns>
      // todo: rename to Equal
      public static Boolean RewritesEqual(SimpleRegex left, SimpleRegex right)
      {
         SimpleRegex.CloneAndRewrite(ref left, ref right);
         return left.SemanticEquals(new HashSet<Pair>(), right);
         //return left.SemanticEquals(right);
      }

      /// <summary>
      /// Determines if the given regexes share a common prefix. Internally clones and rewrites.
      /// </summary>
      /// <param name="left"></param>
      /// <param name="right"></param>
      /// <returns></returns>
      public static Boolean RewritesCommonPrefix(SimpleRegex left, SimpleRegex right)
      {
         SimpleRegex.CloneAndRewrite(ref left, ref right);
         return left.SharesCommonPrefixWith(right);
      }

      // todo: compare a|b with b|a gives error...
      // note: does not rewrite, and can change the 
      internal Boolean Intersects(SimpleRegex other)
      {
         // Second change to MS' code: compare with Zero as well:
         SimpleRegex tIntersection = this.Intersect(new Dictionary<Pair, SimpleRegex>(), 0, other);
         return !(Empty.SemanticEquals(tIntersection) || Zero.SemanticEquals(tIntersection));
      }

      internal Boolean SharesCommonPrefixWith(SimpleRegex other)
      {
         // Second change to MS' code: compare with Zero as well:
         SimpleRegex tPrefixIntersection = this.GetCommonPrefix(new Dictionary<Pair, SimpleRegex>(), 0, other);
         return !(Empty.SemanticEquals(tPrefixIntersection) || Zero.SemanticEquals(tPrefixIntersection));
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
         return Clone().Rewrite().EqualsConsistentHashCode(new Dictionary<SimpleRegex, Int32>(), 1);
      }

      // todo: this really sucks balls, but I'll address that later
      public Int32 EqualsConsistentHashCodeNoRewrite()
      {
         return Clone().EqualsConsistentHashCode(new Dictionary<SimpleRegex, Int32>(), 1);
      }

      // Every regex is:
      // Sum(partial_derivs) | empty | zero
      // Can we do better?
      // this is shitty.. and maps too many hashcodes on one, must improve..
      // todo: must seriously do better here
      protected Int32 EqualsConsistentHashCode(Dictionary<SimpleRegex, Int32> env, Int32 cnt)
      {
         // No effect on hashocde
         if (this.IsZero() || this == Empty)
            return 0;

         Char[] tLetters = Sigma(); 
         
         // todo: must we sort?
         Array.Sort(tLetters);

         if (env.ContainsKey(this))
            return env[this];

         Int32 tResult = cnt;
         env[this] = 2; // don't care, pick this

         if (this is RangeRegex)
         {
            RangeRegex tSelf = (RangeRegex)this;

            // [a-g] = a | b | ... | g
            // pd(c) = (c | (a | b | d | .. | g))(c) = { empty } + {} = { empty } 

         }


         foreach (Char tLetter in tLetters)
         {
            tResult ^= tLetter.GetHashCode();
            tResult ^= Choice(PartialDeriv(tLetter)).EqualsConsistentHashCode(env, cnt);
         }

         return tResult;
      }

      public Boolean SemanticEquals(SimpleRegex other)
      {
         return Clone().SemanticEquals(new HashSet<Pair>(), other.Clone());
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

      // Rewrites both in the same "context".
      // Returns *new* regular expressions.
      public static void CloneAndRewrite(ref SimpleRegex left, ref SimpleRegex right)
      {
         // for now, simply create a choice, this may not suffice if we ever decide to do more
         // clever rewriting, eg by optimizing the tree
         SimpleRegex tResult = Choice(left.Clone(), right.Clone()).Rewrite();
         left = ((ChoiceRegex)tResult).Left;
         right = ((ChoiceRegex)tResult).Right;
      }


      // Rewriting support for ranges and negation.
      // Does not clone, changes tree.
      // todo: instead, clone here rather than above?
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
            if (r is RangeRegex)
               return Choice(from recon in Reconstruct((RangeRegex)r, tDisjointRanges) select
                                 ((RangeRegex)r).Negated ? 
                                    (SimpleRegex)NegatedLetter(tRangeMarkerMap[recon]) :
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

      #region Parser

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
      public static SimpleRegex Parse(String regexExpr)
      {
         Int32 tPos = 0;
         try
         {
            SimpleRegex tResult = Parse_Regex(regexExpr, ref tPos);
            if (tPos != regexExpr.Length)
               throw new Exception("expected end of input");

            return tResult;
         }
         catch (Exception e)
         {
            throw new Exception("Parse error for regex '" + regexExpr + "' around position " + tPos + ": " + e.Message, e);
         }
      }

      protected static SimpleRegex Parse_Regex(String expr, ref Int32 pos)
      {
         SimpleRegex left = Parse_SimpleRegex(expr, ref pos);
         if (pos < expr.Length && expr[pos] == '|')
         {
            pos += 1;
            return Choice(left, Parse_Regex(expr, ref pos));
         }
         else
            return left;
      }

      protected static SimpleRegex Parse_SimpleRegex(String expr, ref Int32 pos)
      {
         SimpleRegex left = Parse_BasicRegex(expr, ref pos);
         if (pos < expr.Length && expr[pos] != ')' && expr[pos] != '|') // lookahead for ) and |
            return Sequence(left, Parse_SimpleRegex(expr, ref pos));
         else
            return left;
      }

      protected static SimpleRegex Parse_BasicRegex(String expr, ref Int32 pos)
      {
         SimpleRegex tNotRegex = Parse_NotRegex(expr, ref pos);
         if (pos < expr.Length)
         {
            if (expr[pos] == '+')
            {
               pos += 1;
               return Sequence(tNotRegex, Star(tNotRegex));
            }
            else if (expr[pos] == '?')
            {
               pos += 1;
               return Choice(Empty, tNotRegex);
            }
            else if (expr[pos] == '*')
            {
               pos += 1;
               return Star(tNotRegex);
            }
            else if (expr[pos] == '{')
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

      protected static SimpleRegex Parse_NotRegex(String expr, ref Int32 pos)
      {
         Boolean tNegate = false;

         if (expr[pos] == '~')
         {
            tNegate = true;
            pos += 1;
         }

         SimpleRegex tRegex = Parse_ElemRegex(expr, ref pos);

         return tNegate ? tRegex.Negate() : tRegex;
      }

      protected static SimpleRegex Parse_ElemRegex(String expr, ref Int32 pos)
      {
         if (expr[pos] == '(')
         {
            pos += 1;
            SimpleRegex tResult = Parse_Regex(expr, ref pos);
            pos += 1; // skip ')' (check?)
            return tResult;
         }
         else if (expr[pos] == '.')
         {
            pos += 1;
            return Complete;
         }
         else if (expr[pos] == '[')
            return Parse_Set(expr, ref pos);
         else
         {
            // no escaping, for now.
            return Letter(ReadLetter(expr, ref pos));
         }
      }

      //protected static readonly Char[] RegexChars = new[] { '[', ']', '(', ')', '^', '.', '{', '}', '*', '+', '-', '?', '|', '&', '\\' };

      // obviously this thing aint readonly
      protected static readonly HashSet<Char> RegexChars = new HashSet<char>()
      {
         '[', ']', '(', ')', '^', '.', '{', '}', '*', '+', '-', '?', '|', '&', '\\', '$'
      };

      // Support escaping.
      protected static Char ReadLetter(String expr, ref Int32 pos)
      {
         if (expr[pos] == '\\')
         {
            pos += 1;

            if (!RegexChars.Contains(expr[pos]))
               throw new Exception(String.Format("Invalid escaped character: '{0}'.", expr[pos]));

            return expr[pos++];
         }
         else
            return ValidateLetter(expr[pos++]);
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

      protected static SimpleRegex Parse_Set(String expr, ref Int32 pos)
      {
         pos += 1; // skip [
         Boolean tNegate = false;
         if (expr[pos] == '^')
         {
            tNegate = true;
            pos += 1;
         }
         SimpleRegex tResult;
         List<Char> tCharList = new List<char>();
         tCharList.Add(ReadLetter(expr, ref pos));
         if (expr[pos] == '-')
         {
            pos += 1; // skip -
            Char tHi = ReadLetter(expr, ref pos);
            pos += 1; // skip ]
            if (tHi < tCharList[0])
               throw new Exception(String.Format("Invalid range: {0} > {1}.", tCharList[0], tHi));
            tResult = Range(tCharList[0], tHi);
         }
         else
         {
            for (; expr[pos] != ']'; )
               tCharList.Add(ReadLetter(expr, ref pos));
            
            pos += 1; // skip ']'

            tResult = Choice(from n in tCharList select Letter(n));
         }

         return tNegate ? tResult.Negate() : tResult;
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

      public override string ToString()
      {
         return "VAR(" + Value + ")";
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
      internal override HashSet<SimpleRegex> PartialDeriv(char c)
      {
         HashSet<SimpleRegex> tResult = Left.PartialDeriv(c);
         tResult.IntersectWith(Right.PartialDeriv(c));
         return tResult;
      }

      internal override char[] Sigma()
      {
         return Left.Sigma().Merge(Right.Sigma());
      }

      public override string ToString()
      {
         return Left.ToString() + " & " + Right.ToString();
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

      public override string ToString()
      {
         return "(" + Left.ToString() + " | " + Right.ToString() + ")";
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
   }

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

      public override string ToString()
      {
         return Letter.ToString();
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
      internal override SimpleRegex Negate()
      {
         //return Choice(Empty, new NegatedLetterRegex() { Letter = this.Letter });
         return NegatedLetter(this.Letter);
      }
   }

   // won't work because of unwieldy sigma
   // instead we'll rewrite to "unique symbols"
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

      public override string ToString()
      {
         return "~" + Letter;
      }

      public override SimpleRegex Apply(Func<SimpleRegex, SimpleRegex> f)
      {
         return f(this);
      }

      // not(not(a)) -> a
      internal override SimpleRegex Negate()
      {
         return Letter(this.Letter);
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

      public override string ToString()
      {
         return "(" + Left.ToString() + " " + Right.ToString() + ")";
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

      // not (A.B) -> not(A) | A.not(B)
      // verify: not(not(A) | A.not(B)) = not(not(A)) & not(A.not(B))
      //    = not(not(A)) & (not(A) | A.not(not(B)))
      //    = A & (not(A) | A.B)
      //    = A & (A.B) = A.B
      internal override SimpleRegex Negate()
      {
         return Choice(Left.Negate(), Sequence(Left.Clone(), Right.Negate()));
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

      public override string ToString()
      {
         return "(" + Operand.ToString() + ")*";
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
   }

   public class EmptyRegex : SimpleRegex
   {
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

      public override string ToString()
      {
         return "<>";
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
      internal override SimpleRegex Negate()
      {
         //return Complete;  
         return Choice(Complete, Zero);
      }
   }

   // todo: matche everything (1 char at a time) ie = a | b | c ..., excluding empty.
   public class CompleteRegex : SimpleRegex 
   {
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
         return Empty;
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

      public override string ToString()
      {
         return ".";
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
         return tOther != null && tOther.Low == this.Low && tOther.High == this.High; // TODO: must check negation here
      }

      public Boolean OverlapsWith(RangeRegex other)
      {
         return (Low >= other.Low && Low <= other.High) || (High <= other.High && High >= other.Low);
      }

      public override int GetHashCode()
      {
         return Low.GetHashCode() ^ High.GetHashCode(); // todo: add negation into this
      }

      internal override bool IsEmpty()
      {
         return false;
      }

      internal override bool IsZero()
      {
         return false;
      }

      // Can not negate, so rewrite must tackle ranges first, and replace with marker.
      internal override SimpleRegex Negate()
      {
         //Negated = !Negated; // used in rewrite
         //return this;
         RangeRegex tResult = (RangeRegex)this.Clone();
         tResult.Negated = !this.Negated;
         return tResult;
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

      public override string ToString()
      {
         return "[" + (Negated ? "^" : "") + Low + "-" + High + "]";
      }
   }

   public class ZeroRegex : SimpleRegex
   {
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

      public override string ToString()
      {
         return @"\0";
      }

      // again, really a singleton.. 
      public override bool Equals(object obj)
      {
         return this is ZeroRegex;
      }

      public override int GetHashCode()
      {
         return 0;
      }

      public override SimpleRegex Apply(Func<SimpleRegex, SimpleRegex> f)
      {
         return f(this);
      }

      // not(0) = a | b | .. | empty
      // not(not(0)) = not(a) & not(b) & ... & not(empty) = 0
      // 
      internal override SimpleRegex Negate()
      {
         return Choice(Complete, Empty);
      }
   }

   //// todo
   //public class NegatedRegex : Regex
   //{
   //   public Regex Operand;
   //}
}
