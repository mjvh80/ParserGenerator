using System;
using System.Text;
using System.Collections.Generic;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using SimpleRegexIntersector;

namespace SimpleRegexIntersectorTest
{
   [TestClass]
   public class RegexEqualityTest
   {
      private void AssertRegexEquals(String left, String right)
      {
         SimpleRegex tLeft = SimpleRegex.Parse(left);
         SimpleRegex tRight = SimpleRegex.Parse(right);

         if (!SimpleRegex.RewritesEqual(tLeft, tRight))
            throw new Exception(String.Format("{0} is not equal to {1}: assertion failure", left, right));

         // todo: api isnt so nice, and here we do a rewrite again.. but it's for testing
         SimpleRegex.CloneAndRewrite(ref tLeft, ref tRight);

         if (tLeft.EqualsConsistentHashCodeNoRewrite() != tRight.EqualsConsistentHashCodeNoRewrite())
            throw new Exception(String.Format("{0} hashcode not equals consistent with {1}", left, right));

         // end old test

         // Now do the same for our builder, without rewriting.
         // 1. Collect alphabet, and ranges.
         SimpleRegexBuilder tBuilder = _GetBuilder(left, right);

         tLeft = tBuilder.Parse(left);
         tRight = tBuilder.Parse(right);

         if (!tLeft.SemanticEquals(tRight))
            throw new Exception(String.Format("[BUILDER] {0} is not equal to {1}: assertion failure", left, right));

         if (tLeft.EqualsConsistentHashCodeNoRewrite() != tRight.EqualsConsistentHashCodeNoRewrite())
            throw new Exception(String.Format("[BUILDER] {0} hashcode not equals consistent with {1}", left, right));
      }

      private static SimpleRegexBuilder _GetBuilder(String left, String right)
      {
         SimpleRegex tLeft = SimpleRegexBuilder.Default.Parse(left);
         SimpleRegex tRight = SimpleRegexBuilder.Default.Parse(right);

         HashSet<Char> tAlphabet = new HashSet<char>();
         foreach (Char tChar in tLeft.Letters())
            tAlphabet.Add(tChar);
         foreach (Char tChar in tRight.Letters())
            tAlphabet.Add(tChar);

         HashSet<RangeRegex> tRanges = new HashSet<RangeRegex>();
         foreach (RangeRegex tRange in tLeft.GetClonedRanges())
            tRanges.Add(tRange);
         foreach (RangeRegex tRange in tRight.GetClonedRanges())
            tRanges.Add(tRange);

         return new SimpleRegexBuilder(tAlphabet, tRanges);
      }

      private void AssertIntersects(String left, String right)
      {
         SimpleRegex tLeft = SimpleRegex.Parse(left);
         SimpleRegex tRight = SimpleRegex.Parse(right);
         //SimpleRegex.Rewrite(ref tLeft, ref tRight);
         //if (!tLeft.Intersects(tRight))
         if (!SimpleRegex.RewritesIntersect(tLeft, tRight))
            throw new Exception(String.Format("{0} does not intersect {1}: assertion failure", left, right));

         // New Test
         SimpleRegexBuilder tBuilder = _GetBuilder(left, right);
         if (!tBuilder.Parse(left).Intersects(tBuilder.Parse(right)))
            throw new Exception(String.Format("[BUILDER] {0} does not intersect {1}: assertion failure", left, right));
      }

      private void AssertDoesNotIntersect(String left, String right)
      {
         SimpleRegex tLeft = SimpleRegex.Parse(left);
         SimpleRegex tRight = SimpleRegex.Parse(right);
         //SimpleRegex.Rewrite(ref tLeft, ref tRight);
         //if (tLeft.Intersects(tRight))
         if (SimpleRegex.RewritesIntersect(tLeft, tRight))
            throw new Exception(String.Format("{0} intersects {1}: assertion failure", left, right));

         // New Test
         SimpleRegexBuilder tBuilder = _GetBuilder(left, right);
         if (tBuilder.Parse(left).Intersects(tBuilder.Parse(right)))
            throw new Exception(String.Format("[BUILDER] {0} intersects {1}: assertion failure", left, right));
      }

      private void AssertHaveCommonPrefix(String left, String right)
      {
         SimpleRegex tLeft = SimpleRegex.Parse(left);
         SimpleRegex tRight = SimpleRegex.Parse(right);
         //SimpleRegex.Rewrite(ref tLeft, ref tRight);
         //if (!tLeft.SharesCommonPrefixWith(tRight))
         if (!SimpleRegex.RewritesCommonPrefix(tLeft, tRight))
            throw new Exception(String.Format("{0} does not share a common prefix with {1}: assertion failure", left, right));

         // New Test
         SimpleRegexBuilder tBuilder = _GetBuilder(left, right);
         if (!tBuilder.Parse(left).SharesCommonPrefixWith(tBuilder.Parse(right)))
            throw new Exception(String.Format("[BUILDER] {0} does not share a common prefix with {1}: assertion failure", left, right));
      }

      [TestMethod]
      public void TestRegexEquality()
      {
         AssertRegexEquals(".", "~a|a");
         AssertRegexEquals("a", "a");
         AssertRegexEquals("~a", "~a");
         AssertRegexEquals("abc", "[a-a]bc");
         AssertRegexEquals("a|b", "[a-b]");
         AssertRegexEquals("a+", "aa*");
         AssertRegexEquals("a|b", "b|a");
         AssertRegexEquals("a?a*", "a*");
      }

      [TestMethod]
      public void TestRegexIntersections()
      {
         AssertIntersects(".", "a");
         AssertIntersects(".", "~a");
         AssertIntersects(".", "~a|a");
         AssertIntersects("[a-z]", "c");
         AssertIntersects("a|b|c|d", "c");
         AssertIntersects("a*", "a+");
         AssertIntersects("a?", "a+");
         AssertIntersects("~[a-c]", "d");
         AssertIntersects("~(a|b)", "d");
         AssertIntersects("abcd", "a[b-c]*d");
      }

      [TestMethod]
      public void TestRegexNoIntersections()
      {
         AssertDoesNotIntersect("a", "aa"); // caused trouble, introduced Zero into semantic equality check
         AssertDoesNotIntersect("a", "b");
         AssertDoesNotIntersect("a|b", "c|d");
         AssertDoesNotIntersect("~a", "a");
         AssertDoesNotIntersect("~(a|b)", "a|b*");
         AssertDoesNotIntersect("a|b|c", "[g-m]");

         AssertDoesNotIntersect("[0-9]+", @"(\.[0-9]+)|([0-9]+\.[0-9]*)");
         AssertDoesNotIntersect("[0-9]+", @"([0-9]+\.[0-9]*)");

         // AA*, AA*. and AA*.A*
         AssertDoesNotIntersect("A*", @"A*\.");
      }

      [TestMethod]
      public void TestRegexPrefixIntersections()
      {
         AssertHaveCommonPrefix("a", "aa");
         //AssertHaveCommonPrefix("a*", "b*"); //?
         AssertHaveCommonPrefix("[a-z][b-p]obar", "foo");
         AssertHaveCommonPrefix("[a-z]oo", "fo");
         AssertHaveCommonPrefix("~a", "b");
         AssertHaveCommonPrefix("ab", "ad"); // caused trouble
      }

      [TestMethod]
      public void HashCodeTests()
      {
      //   Assert.AreEqual(SimpleRegex.Parse("a").EqualsConsistentHashCode(), SimpleRegex.Parse("a").EqualsConsistentHashCode());
      //   Assert.AreEqual(SimpleRegex.Parse("a|b").EqualsConsistentHashCode(), SimpleRegex.Parse("b|a").EqualsConsistentHashCode());
         Assert.AreEqual(SimpleRegex.Parse("aa*aa").EqualsConsistentHashCode(), SimpleRegex.Parse("aaa*a").EqualsConsistentHashCode());
         Assert.AreEqual(SimpleRegex.Parse("a").EqualsConsistentHashCode(), SimpleRegex.Parse("a").EqualsConsistentHashCode());
         Assert.AreEqual(SimpleRegex.Parse("a").EqualsConsistentHashCode(), SimpleRegex.Parse("a").EqualsConsistentHashCode());
      }
   }
}
