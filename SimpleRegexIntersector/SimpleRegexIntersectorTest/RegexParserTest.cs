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
         Regex.Parse("a");
         Regex.Parse("a|b");
         Regex.Parse("ab");
         Regex.Parse("a*");
         Regex.Parse("a?");
         Regex.Parse("a+");
         Regex.Parse("a|(ab)");
         Regex.Parse("\a");
         Regex.Parse("[a-b]");
         Regex.Parse("[\a-\b]");
         Regex.Parse("[^a-b]");
         Regex.Parse("[ab]");
         Regex.Parse("[a]");
         Regex.Parse("[^a]");
         Regex.Parse("[^a\b]");
         Regex.Parse("[^\a]");
      }

      [TestMethod]
      public void TestBadGrammar()
      {
         AssertThrows(() => Regex.Parse(""));
         AssertThrows(() => Regex.Parse("~"));
         AssertThrows(() => Regex.Parse("a)"));
         AssertThrows(() => Regex.Parse("()"));
         AssertThrows(() => Regex.Parse("[]"));
         AssertThrows(() => Regex.Parse("[b-a]")); // invalid range
         AssertThrows(() => Regex.Parse("(a"));
         AssertThrows(() => Regex.Parse("[a-b"));
      }
   }
}
