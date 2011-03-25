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
            Assert.Fail("Assertion failure: expected exception of type " + typeof(T));
         }
         catch (AssertFailedException)
         {
            throw;
         }
         catch (T)
         {
            // ok
         }
      }

      [TestMethod]
      public void TestEscaping()
      {
         // '[', ']', '(', ')', '^', '.', '{', '}', '*', '+', '-', '?', '|', '&', '\\'
         Assert.AreEqual("foobar", SimpleRegex.Escape("foobar"));
         Assert.AreEqual("\\[\\]\\(\\)\\^\\.\\{\\}\\*\\+\\-\\?\\|\\&\\\\", SimpleRegex.Escape("[]()^.{}*+-?|&\\"));
         Assert.AreEqual("[]()^.{}*+-?|&\\", SimpleRegex.Unescape(SimpleRegex.Escape("[]()^.{}*+-?|&\\")));
         Assert.AreEqual("\\\\\\\\", SimpleRegex.Escape("\\\\"));
         Assert.AreEqual("", SimpleRegex.Escape(""));
         Assert.AreEqual(null, SimpleRegex.Escape(null));

         Assert.AreEqual("foobar", SimpleRegex.Unescape(SimpleRegex.Escape("foobar")));

         Assert.AreEqual(null, SimpleRegex.Unescape(null));
         Assert.AreEqual("", SimpleRegex.Unescape(""));

         AssertThrows(() => SimpleRegex.Unescape("\\o"));
         AssertThrows(() => SimpleRegex.Unescape("\\"));
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
         SimpleRegex foo = SimpleRegex.Parse("[^\"]");
         SimpleRegex.Parse("'[^']*'");
        SimpleRegex.Parse("'([^']|\\\\')*'");
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
