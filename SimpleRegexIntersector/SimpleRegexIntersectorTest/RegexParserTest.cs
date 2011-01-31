using System;
using System.Text;
using System.Collections.Generic;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using SimpleRegexIntersector;

namespace SimpleRegexIntersectorTest
{
   [TestClass]
   public class RegexParserTest
   {
      public static void AssertThrows(Action a) { AssertThrows<Exception>(a); }
      public static void AssertThrows<T>(Action a) where T : Exception
      {
         try
         {
            a();
            throw new Exception("Assertion failure: expected exception of type " + typeof(T));
         }
         catch (T)
         {
            // ok
         }
         catch
         {
            throw; // not ok
         }
      }

      [TestMethod]
      public void TestCorrectGrammar()
      {
         SimpleRegex.Parse("a");
         SimpleRegex.Parse("a|b");
         SimpleRegex.Parse("ab");
         SimpleRegex.Parse("a*");
         SimpleRegex.Parse("a?");
         SimpleRegex.Parse("a+");
         SimpleRegex.Parse("a|(ab)");
         SimpleRegex.Parse("\a");
         SimpleRegex.Parse("[a-b]");
         SimpleRegex.Parse("[\a-\b]");
         SimpleRegex.Parse("[^a-b]");
         SimpleRegex.Parse("[ab]");
         SimpleRegex.Parse("[a]");
         SimpleRegex.Parse("[^a]");
         SimpleRegex.Parse("[^a\b]");
         SimpleRegex.Parse("[^\a]");
      }

      [TestMethod]
      public void TestBadGrammar()
      {
         AssertThrows(() => SimpleRegex.Parse(""));
         AssertThrows(() => SimpleRegex.Parse("~"));
         AssertThrows(() => SimpleRegex.Parse("a)"));
         AssertThrows(() => SimpleRegex.Parse("()"));
         AssertThrows(() => SimpleRegex.Parse("[]"));
         AssertThrows(() => SimpleRegex.Parse("[b-a]")); // invalid range
         AssertThrows(() => SimpleRegex.Parse("(a"));
         AssertThrows(() => SimpleRegex.Parse("[a-b"));
      }
   }
}
