﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

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


   // todo: other extensions possible:
   // ^: match start of input: requires no work, just ensure with parsing that ^ can not occur as char
   // $: match end of input: - " -

   public abstract class SimpleRegex
   {
      public static SimpleRegex Empty = new EmptyRegex();
      public static SimpleRegex Zero = new ZeroRegex();
      public static SimpleRegex Complete = new CompleteRegex();

      /// <summary>
      /// Returns, for regex f, the set { p | (x, p) in linear forms of f }.
      /// </summary>
      /// <param name="c"></param>
      /// <returns></returns>
      public abstract HashSet<SimpleRegex> PartialDeriv(Char c);
      public abstract Char[] Sigma();
      public abstract Boolean IsEmpty();
      public abstract Boolean IsZero();

      // Return negation of self, in terms of others.
      public abstract SimpleRegex Negate();

      // Util for rewriting the tree.
      public abstract SimpleRegex Apply(Func<SimpleRegex, SimpleRegex> f);
      
      public static Pair Pair(SimpleRegex left, SimpleRegex right) { return new Pair() { Left = left, Right = right }; }
      public static ChoiceRegex Choice(SimpleRegex left, SimpleRegex right) { return new ChoiceRegex() { Left = left, Right = right }; }
      public static KleeneRegex Star(SimpleRegex op) { return new KleeneRegex() { Operand = op }; }
      public static SeqRegex Sequence(SimpleRegex left, SimpleRegex right) { return new SeqRegex() { Left = left, Right = right }; }
      public static VarRegex Var(Int32 x) { return new VarRegex() { Value = x }; }
      public static LetterRegex Letter(Char c) { return new LetterRegex() { Letter = c }; }
      public static NegatedLetterRegex NegatedLetter(Char c) { return new NegatedLetterRegex() { Letter = c }; }
      public static AndRegex And(SimpleRegex left, SimpleRegex right) { return new AndRegex() { Left = left, Right = right }; }
      public static SimpleRegex Not(SimpleRegex op) { return op.Negate(); }
      public static RangeRegex Range(Char lo, Char hi) { return new RangeRegex() { Low = lo, High = hi }; }

      /* Performs a conversion to remove recursion:
       * 
       * (r) x (r1) = r2 => x =?
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
       */
      public abstract Pair ConvertInternal(Int32 x);
      public abstract Boolean Contains(Int32 x);

      public SimpleRegex Convert(Int32 x)
      {
         Pair tConversion = this.ConvertInternal(x);
         return Sequence(Star(tConversion.Left), tConversion.Right);
      }

      public static SimpleRegex Choice(IEnumerable<SimpleRegex> pList, params SimpleRegex[] others)
      {
         //return ToRegex(pList.GetEnumerator());
         return ToRegex((pList.Union(others).GetEnumerator()));
      }

      public static SimpleRegex ToRegex(IEnumerator<SimpleRegex> pList)
      {
         if (pList.MoveNext())
            return Choice(pList.Current, ToRegex(pList));
         else
            return Zero;
      }

      // Performs a rewrite and intersects.
      public static Boolean RewritesIntersect(SimpleRegex left, SimpleRegex right)
      {
         SimpleRegex.Rewrite(ref left, ref right);
         return left.Intersects(right);
      }

      public static Boolean RewritesEqual(SimpleRegex left, SimpleRegex right)
      {
         SimpleRegex.Rewrite(ref left, ref right);
         return left.SemanticEquals(right);
      }

      public static Boolean RewritesCommonPrefix(SimpleRegex left, SimpleRegex right)
      {
         SimpleRegex.Rewrite(ref left, ref right);
         return left.SharesCommonPrefixWith(right);
      }

      // todo: compare a|b with b|a gives error...
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

      public SimpleRegex Intersect(Dictionary<Pair, SimpleRegex> env, Int32 x, SimpleRegex r2)
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

            // Remove the regex for this recursion level. Adjust M.S. algorithm.
            env.Remove(Pair(this, r2));
            return tFinal.Convert(x);
         }
      }

      // R = A B
      public SimpleRegex GetCommonPrefix(Dictionary<Pair, SimpleRegex> env, Int32 x, SimpleRegex r2)
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
         foreach (Char tLetter in tLetters)
         {
            tResult ^= tLetter.GetHashCode();
            tResult ^= Choice(PartialDeriv(tLetter)).EqualsConsistentHashCode(env, cnt);
         }

         return tResult;
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

      protected IEnumerable<RangeRegex> Split(RangeRegex range, RangeRegex comp)
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
      protected void AddRange(RangeRegex range, HashSet<RangeRegex> disjointSet)
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
                     AddRange(tDisjoint, disjointSet);
               }
         }
      }

      protected IEnumerable<RangeRegex> Reconstruct(RangeRegex range, HashSet<RangeRegex> disjointRanges)
      {
         foreach (RangeRegex tDisjointRange in disjointRanges)
            if (range.Contains(tDisjointRange))
               yield return tDisjointRange;
      }

      // again, implemented naively, ill focus on some perf later...
      // to improve: used a sorted list of letters and iterate through once
      protected IEnumerable<RangeRegex> SplitOnLetters(RangeRegex range, IEnumerable<Char> letters)
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
      public static void Rewrite(ref SimpleRegex left, ref SimpleRegex right)
      {
         // for now, simply create a choice, this may not suffice if we ever decide to do more
         // clever rewriting, eg by optimizing the tree
         SimpleRegex tResult = Choice(left, right).Rewrite();
         left = ((ChoiceRegex)tResult).Left;
         right = ((ChoiceRegex)tResult).Right;
      }

      // Rewriting support for ranges and negation.
      public SimpleRegex Rewrite()
      {
         SimpleRegex tResult = this;

         HashSet<Char> tUsedSet = new HashSet<char>();
         tUsedSet.AddRange(tResult.Sigma());

         // todo: add negated ones to Sigma??
         // all negated letters
         //HashSet<Char> tNegatedSet = new HashSet<char>();
         // Note: no longer do this, used Sigma instead.
         //tResult.Apply(r =>
         //{
         //   if (r is NegatedLetterRegex)
         //      tNegatedSet.Add(((NegatedLetterRegex)r).Letter);

         //   return r;
         //});

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
            AddRange(tRange, tDisjointRanges);
         
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
         Char tMagicMarker = 'Z'; // '\uFADE';

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

      // Support escaping.
      protected static Char ReadLetter(String expr, ref Int32 pos)
      {
         if (expr[pos] == '\\')
         {
            pos += 1;
            // allow the character "as is"
            return expr[pos++];
         }
         else
            return ValidateLetter(expr[pos++]);
      }

      protected static Char ValidateLetter(Char c)
      {
         HashSet<Char> tSyntaxSet = new HashSet<char>();
         tSyntaxSet.AddRange(new [] { '[', ']', '(', ')', '^', '.', '{', '}', '*', '+', '-', '?', '|', '&', '\\' }); // todo dont regen
         if (tSyntaxSet.Contains(c))
            throw new Exception(String.Format("Letter '{0}' is part of regex syntax.", c.ToString()));
         return c;
      }

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

      public override Pair ConvertInternal(int x)
      {
         if (x == Value)
            return Pair(Empty, Empty); // remove
         else
            return Pair(Empty, this);  // keep it
      }

      public override bool Contains(int x)
      {
         return x == Value;
      }

      public override HashSet<SimpleRegex> PartialDeriv(char c)
      {
         throw new InvalidOperationException();
      }

      public override bool IsEmpty()
      {
         throw new InvalidOperationException();
      }

      public override bool IsZero()
      {
         throw new NotImplementedException();
      }

      public override char[] Sigma()
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

      public override SimpleRegex Negate()
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

      public override bool Contains(int x)
      {
         return Left.Contains(x) || Right.Contains(x);
      }

      public override Pair ConvertInternal(int x)
      {
         return Pair(Empty, this); // todo: must figure out what convert does, is this correct?
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

      public override bool IsEmpty()
      {
         return Left.IsEmpty() && Right.IsEmpty();
      }

      public override bool IsZero()
      {
         return Left.IsZero() || Right.IsZero();
      }

      // not (A & B) -> not(A) | not(B)
      public override SimpleRegex Negate()
      {
         return Choice(Left.Negate(), Right.Negate());
      }

      // returns p(left, c) INTERSECT p(right, c)
      public override HashSet<SimpleRegex> PartialDeriv(char c)
      {
         HashSet<SimpleRegex> tResult = Left.PartialDeriv(c);
         tResult.IntersectWith(Right.PartialDeriv(c));
         return tResult;
      }

      public override char[] Sigma()
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

      public override bool Contains(int x)
      {
         return Left.Contains(x) || Right.Contains(x);
      }

      public override HashSet<SimpleRegex> PartialDeriv(Char c)
      {
         return Left.PartialDeriv(c).Merge(Right.PartialDeriv(c));
         //var tLeftDerivs = Left.PartialDeriv(c);
         //var tRightDerivs = Right.PartialDeriv(c);
         //tLeftDerivs.AddRange(tRightDerivs);
         //return tLeftDerivs.Nub();
      }

      public override Pair ConvertInternal(int x)
      {
         Pair tConvLeft = Left.ConvertInternal(x);
         Pair tConvRight = Right.ConvertInternal(x);
         return Pair(Choice(tConvLeft.Left, tConvRight.Left), Choice(tConvLeft.Right, tConvRight.Right));
      }

      public override char[] Sigma()
      {
         return Left.Sigma().Merge(Right.Sigma()).Nub(); // todo: can do merge/nub in one go
      }

      public override bool IsZero()
      {
         return Left.IsZero() && Right.IsZero();
      }

      public override bool IsEmpty()
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
      public override SimpleRegex Negate()
      {
         return And(Left.Negate(), Right.Negate());
      }
   }

   public class LetterRegex : SimpleRegex
   {
      public Char Letter;

      public override HashSet<SimpleRegex> PartialDeriv(Char c)
      {
         if (c == Letter)
            return new HashSet<SimpleRegex>() { SimpleRegex.Empty };
         else
            return new HashSet<SimpleRegex>();
      }

      public override bool Contains(int x)
      {
         return false;
      }

      public override Pair ConvertInternal(int x)
      {
         return Pair(Empty, this);
      }

      public override string ToString()
      {
         return Letter.ToString();
      }

      public override bool IsEmpty()
      {
         return false;
      }

      public override bool IsZero()
      {
         return false;
      }

      public override char[] Sigma()
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
      public override SimpleRegex Negate()
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
      public override HashSet<SimpleRegex> PartialDeriv(char c)
      {
         if (c == Letter)
            return new HashSet<SimpleRegex>(); // empty set
         else
            return new HashSet<SimpleRegex>() { Empty };
      }

      public override bool Contains(int x)
      {
         return false;
      }

      public override Pair ConvertInternal(int x)
      {
         return Pair(Empty, this); // todo...
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

      public override bool IsEmpty()
      {
         return false;
      }

      public override bool IsZero()
      {
         return false;
      }

      public override char[] Sigma()
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
      public override SimpleRegex Negate()
      {
         return Letter(this.Letter);
      }
   }

   public class SeqRegex : SimpleRegex
   {
      public SimpleRegex Left, Right;

      public override HashSet<SimpleRegex> PartialDeriv(char c)
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

      public override bool Contains(int x)
      {
         return Left.Contains(x) || Right.Contains(x);
      }

      public override Pair ConvertInternal(int x)
      {
         if (Right.Contains(x))
         {
            Pair tRightPair = Right.ConvertInternal(x);
            return Pair(new SeqRegex() { Left = this.Left, Right = tRightPair.Left }, tRightPair.Right);
         }
         else
            return Pair(Empty, this);
      }

      public override bool IsEmpty()
      {
         return Left.IsEmpty() && Right.IsEmpty();
      }

      public override bool IsZero()
      {
         return Left.IsZero() || Right.IsZero();
      }

      public override string ToString()
      {
         return "(" + Left.ToString() + " " + Right.ToString() + ")";
      }

      public override char[] Sigma()
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
      public override SimpleRegex Negate()
      {
         return Choice(Left.Negate(), Sequence(Left, Right.Negate()));
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

      public override HashSet<SimpleRegex> PartialDeriv(char c)
      {
         HashSet<SimpleRegex> tDerivs = new HashSet<SimpleRegex>();
         foreach (SimpleRegex tPart in Operand.PartialDeriv(c))
            tDerivs.Add(new SeqRegex() { Left = tPart, Right = this });
         return tDerivs;
      }

      public override bool Contains(int x)
      {
         return Operand.Contains(x);
      }

      public override Pair ConvertInternal(int x)
      {
         return Pair(Empty, this); // todo: im not entirely sure if correct, but i think it is
      }

      public override bool IsEmpty()
      {
         return true;
      }

      public override bool IsZero()
      {
         return false;
      }

      public override string ToString()
      {
         return "(" + Operand.ToString() + ")*";
      }

      public override char[] Sigma()
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
      public override SimpleRegex Negate()
      {
         return And(Empty.Negate(), Star(Operand.Negate()));
      }
   }

   public class EmptyRegex : SimpleRegex
   {
      public override HashSet<SimpleRegex> PartialDeriv(char c)
      {
         return new HashSet<SimpleRegex>();
      }

      public override Pair ConvertInternal(int x)
      {
         return Pair(Empty, Empty);
      }

      public override bool Contains(int x)
      {
         return false;
      }

      public override bool IsEmpty()
      {
         return true;
      }

      public override bool IsZero()
      {
         return false;
      }

      public override string ToString()
      {
         return "<>";
      }

      public override char[] Sigma()
      {
         return new Char[] { };
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
      public override SimpleRegex Negate()
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

      public override bool Contains(int x)
      {
         return false;
      }

      public override Pair ConvertInternal(int x)
      {
         throw new NotImplementedException();
      }

      public override bool Equals(object obj)
      {
         return obj is CompleteRegex; // todo: check singleton?
      }

      public override int GetHashCode()
      {
         return 234978;
      }

      public override bool IsEmpty()
      {
         return false;
      }

      public override bool IsZero()
      {
         return false;
      }

      // not(a|b|..|z) = not(a) & not(b) ... & not(z)
      //               = (n(a) & n(b) ... n(z) ) | Empty = Zero | Empty = Empty
      public override SimpleRegex Negate()
      {
         return Empty;
      }

      public override HashSet<SimpleRegex> PartialDeriv(char c)
      {
         throw new NotImplementedException();
      }

      // can't meaningfully implement, anyway, this class is intended for rewriting only.
      public override char[] Sigma()
      {
         return new Char[0];// 
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

      public override bool Contains(int x)
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

      public override Pair ConvertInternal(int x)
      {
         throw new NotImplementedException();
      }

      public Int32 Length { get { return ((Int32)(High - Low) + 1); } }

      public override bool Equals(object obj)
      {
         RangeRegex tOther = obj as RangeRegex;
         return tOther != null && tOther.Low == this.Low && tOther.High == this.High;
      }

      public Boolean OverlapsWith(RangeRegex other)
      {
         return (Low >= other.Low && Low <= other.High) || (High <= other.High && High >= other.Low);
      }

      public override int GetHashCode()
      {
         return Low.GetHashCode() ^ High.GetHashCode();
      }

      public override bool IsEmpty()
      {
         return false;
      }

      public override bool IsZero()
      {
         return false;
      }

      // Can not negate, so rewrite must tackle ranges first, and replace with marker.
      public override SimpleRegex Negate()
      {
         Negated = !Negated; // used in rewrite
         return this;
      }

      public override HashSet<SimpleRegex> PartialDeriv(char c)
      {
         throw new NotImplementedException();
      }

      public override char[] Sigma()
      {
         // rewrite intentions, no sigma here
         return new Char[0];
      }

      public override string ToString()
      {
         return "[" + (Negated ? "^" : "") + Low + "-" + High + "]";
      }
   }

   public class ZeroRegex : SimpleRegex
   {
      public override HashSet<SimpleRegex> PartialDeriv(char c)
      {
         return new HashSet<SimpleRegex>();
      }

      public override Pair ConvertInternal(int x)
      {
         return Pair(Empty, Zero); // todo: not sure correct
      }

      public override bool Contains(int x)
      {
         return false;
      }

      public override bool IsEmpty()
      {
         return false;
      }

      public override bool IsZero()
      {
         return true;
      }

      public override char[] Sigma()
      {
         return new Char[] { };
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
      public override SimpleRegex Negate()
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