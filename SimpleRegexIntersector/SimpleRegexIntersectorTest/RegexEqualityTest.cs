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
         SimpleRegex.Rewrite(ref tLeft, ref tRight);
         if (!tLeft.SemanticEquals(tRight))
            throw new Exception(String.Format("{0} is not equal to {1}: assertion failure", left, right));
      }

      private void AssertIntersects(String left, String right)
      {
         SimpleRegex tLeft = SimpleRegex.Parse(left);
         SimpleRegex tRight = SimpleRegex.Parse(right);
         SimpleRegex.Rewrite(ref tLeft, ref tRight);
         if (!tLeft.Intersects(tRight))
            throw new Exception(String.Format("{0} is not equal to {1}: assertion failure", left, right));
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
   }
}
