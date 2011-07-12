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
      private static void AssertRegexEquals(String left, String right)
      {
         // Now do the same for our builder, without rewriting.
         // 1. Collect alphabet, and ranges.
         SimpleRegexBuilder tBuilder = _GetBuilder(left, right);

         SimpleRegex tLeft = tBuilder.Parse(left);
         SimpleRegex tRight = tBuilder.Parse(right);

         if (!tLeft.SemanticEquals(tRight))
            throw new Exception(String.Format("[BUILDER] {0} is not equal to {1}: assertion failure", left, right));

         if (tLeft.EqualsConsistentHashCode() != tRight.EqualsConsistentHashCode())
            throw new Exception(String.Format("[BUILDER] {0} hashcode not equals consistent with {1}", left, right));

         // Also test cloning works.
         SimpleRegex tClone = tLeft.Clone();
         if (Object.ReferenceEquals(tClone, tLeft))
            throw new Exception("Bad clone: it is equal to itself by reference.");

         if (!tLeft.Clone().SemanticEquals(tLeft))
            throw new Exception(String.Format("[BUILDER] {0} clone is not equal to itself: assertion failure", left));
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

         return new SimpleRegexBuilder(tAlphabet, tRanges).Build();
      }

      private void AssertIntersects(String left, String right)
      {
         // New Test
         SimpleRegexBuilder tBuilder = _GetBuilder(left, right);
         AssertIntersects(tBuilder.Parse(left), tBuilder.Parse(right));
      }

      private void AssertIntersects(SimpleRegex left, SimpleRegex right)
      {
         if (!left.Intersects((right)))
            throw new Exception(String.Format("[BUILDER] {0} does not intersect {1}: assertion failure", left, right));
      }

      private void AssertDoesNotIntersect(String left, String right)
      {
         // New Test
         SimpleRegexBuilder tBuilder = _GetBuilder(left, right);
         AssertDoesNotIntersect(tBuilder.Parse(left), tBuilder.Parse(right));
      }

      private void AssertDoesNotIntersect(SimpleRegex left, SimpleRegex right)
      {
         if (left.Intersects(right))
            throw new Exception(String.Format("[BUILDER] {0} intersects {1}: assertion failure", left, right));
      }

      private void AssertHaveCommonPrefix(String left, String right)
      {
         // New Test
         SimpleRegexBuilder tBuilder = _GetBuilder(left, right);
         if (!tBuilder.Parse(left).SharesCommonPrefixWith(tBuilder.Parse(right)))
            throw new Exception(String.Format("[BUILDER] {0} does not share a common prefix with {1}: assertion failure", left, right));
      }

      private void AssertHashCodesEqual(String left, String right)
      {
         SimpleRegexBuilder tBuilder = _GetBuilder(left, right);
         if (tBuilder.Parse(left).EqualsConsistentHashCode() != tBuilder.Parse(right).EqualsConsistentHashCode())
            throw new Exception(String.Format("[BUILDER] Hashcodes do not match for {0} and {1}.", left, right));
      }

      private void AssertHashCodesDoNotEqual(String left, String right)
      {
         SimpleRegexBuilder tBuilder = _GetBuilder(left, right);
         if (tBuilder.Parse(left).EqualsConsistentHashCode() == tBuilder.Parse(right).EqualsConsistentHashCode())
            throw new Exception(String.Format("[BUILDER] Hashcodes do not match for {0} and {1}.", left, right));
      }

      [TestMethod]
      public void TestRegexEquality()
      {
         AssertRegexEquals(".*", "~a|a");
         AssertRegexEquals("a", "a");
         AssertRegexEquals("~a", "~a");
         AssertRegexEquals("abc", "[a-a]bc");
         AssertRegexEquals("a|b", "[a-b]");
         AssertRegexEquals("a+", "aa*");
         AssertRegexEquals("a|b", "b|a");
         AssertRegexEquals("a?a*", "a*");
         AssertRegexEquals("(a|[^a])b", ".b"); // todo: needs work in range rewriting which should not contain empty !
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
         AssertDoesNotIntersect("~[a-d]", "d");
         AssertIntersects("~(a|b)", "d");
         AssertIntersects("abcd", "a[b-c]*d");
         AssertIntersects("~(a|b)", "a|b*"); // note that bb matches both

         SimpleRegexBuilder tBuilder = _GetBuilder("(a|b|c|d)", "~e");
         SimpleRegex tLeft = tBuilder.Parse("(a|b|c|d)");
         SimpleRegex tRight = tBuilder.AndNot(tLeft, tBuilder.Parse("e"));
         AssertIntersects(tLeft, tRight); // actually succeeds without rewrite/normalization

         tBuilder = _GetBuilder("[a-z]", "~a");
         tLeft = tBuilder.Parse("[a-z]");
         tRight = tBuilder.AndNot(tLeft, tBuilder.Parse("a"));
         AssertIntersects(tLeft, tRight);

         // One that was failing.
         String tNameStartChar = "[a-e]";
         String tNameChar = "[0-9]";

         String tQName = "[a-d]*"; // String.Format("[a-z]*");// String.Format("({0})({0}|{1})*(:({0})({0}|{1})*)?", tNameStartChar, tNameChar);
         tBuilder = _GetBuilder(tQName, "cd|ba");
         tLeft = tBuilder.AndNot(tBuilder.Parse(tQName), tBuilder.Parse("cd"));
         tRight = tBuilder.Parse("ba");
         var tCd = tBuilder.Parse("cd");
         AssertDoesNotIntersect(tLeft, tCd);
         AssertIntersects(tLeft, tRight);
      }

      [TestMethod]
      public void TestRegexNoIntersections()
      {
         AssertDoesNotIntersect("a", "aa"); // caused trouble, introduced Zero into semantic equality check
         AssertDoesNotIntersect("a", "b");
         AssertDoesNotIntersect("a|b", "c|d");
         AssertDoesNotIntersect("~a", "a");
         
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
         AssertHashCodesEqual("a|b", "b|a");

         AssertHashCodesEqual("a..b", "(a|a)..b");
         AssertHashCodesEqual("a|a|a|a", "a");

         AssertHashCodesEqual("[a-c]*", "(a|b|c)*");

         // It does not matter if they are not equal.
   //      AssertHashCodesDoNotEqual("aa*aa", "aaa*a");
         AssertHashCodesEqual("a", "a");
      }
   }
}
