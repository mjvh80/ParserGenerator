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
         }
         catch (AssertFailedException)
         {
            throw;
         }
         catch (T)
         {
            return; // OK
         }

         Assert.Fail("Assertion failure: expected exception of type " + typeof(T));
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

         // Add some whitespace.
         SimpleRegex.Parse(" a ");
         SimpleRegex.Parse(" a |  b  ");
         SimpleRegex.Parse(" a  b");
         SimpleRegex.Parse("a *");
         SimpleRegex.Parse("a  ?");
         SimpleRegex.Parse("a  +");
         SimpleRegex.Parse("a | (  ab )  ");
         SimpleRegex.Parse(" \a");
         SimpleRegex.Parse(" [a-b] ");
         SimpleRegex.Parse(" [\a-\b] ");
         SimpleRegex.Parse(" [^a-b] ");
         SimpleRegex.Parse(" [ab] ");
         SimpleRegex.Parse(" [a] ");
         SimpleRegex.Parse(" [a b] ");
         SimpleRegex.Parse(" [^a] ");
         SimpleRegex.Parse(" [^a\b] ");
         SimpleRegex.Parse(" [^\a] ");
         SimpleRegex.Parse(" [^\"] ");
         SimpleRegex.Parse(" '[^'] *' ");
         SimpleRegex.Parse(" '([^'] | \\\\') *' ");
      }

      [TestMethod]
      public void TestWhitespaceIgnored()
      {
         AssertThrows(() => SimpleRegex.Parse("              ")); // equivalent to an empty regex
         Assert.AreEqual(SimpleRegex.Letter('a'), SimpleRegex.Parse("   a  "));
         Assert.AreEqual(SimpleRegex.Star(SimpleRegex.Letter('a')), SimpleRegex.Parse(" a  * "));
         Assert.AreEqual(SimpleRegex.Choice(SimpleRegex.Letter('a'), SimpleRegex.Letter('b')), SimpleRegex.Parse(" a |   b "));
         
         // Test whitespace remains in sets:
         Assert.AreEqual(SimpleRegex.Range(' ', 'z'), SimpleRegex.Parse("  [ -z] "));
         Assert.AreEqual(SimpleRegex.Choice(from c in new[] { 'a', ' ', 'b' } select SimpleRegex.Letter(c)), SimpleRegex.Parse(" [a b] "));

         SimpleRegex.Parse("[ ]"); // ok
      }

      [TestMethod]
      public void TestWhitespaceSignificant()
      {
         Assert.AreEqual(SimpleRegex.Letter(' '), SimpleRegex.Parse(" ", 0));
         Assert.AreEqual(SimpleRegex.Sequence(SimpleRegex.Letter('a'), SimpleRegex.Letter(' ')), SimpleRegex.Parse("a ", 0));
         //Assert.AreEqual(SimpleRegex.Sequence(SimpleRegex.Letter('a'), SimpleRegex.Letter(' ')

         // Invalid range.
         AssertThrows(() => SimpleRegex.Parse("[a- ]", 0));
          
         // Ok
         SimpleRegex.Parse("[ ]", 0);
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
