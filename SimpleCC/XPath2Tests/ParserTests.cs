using System;
using System.Text;
using System.Collections.Generic;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using XPath2;

namespace XPath2Tests
{
   [TestClass]
   public class ParserTests
   {
      [TestMethod]
      public void TestMethod1()
      {
         Parser tParser = new Parser();

         Assert.IsTrue(tParser.Parse("foo"));
         Assert.IsFalse(tParser.Parse("bar"));

         Assert.IsTrue(tParser.Parse("if (foo  ) then foo else foo"));
         Assert.IsTrue(tParser.Parse("for $foo in foo return foo"));

         Assert.IsTrue(tParser.Parse("if then bar"));
      }
   }
}
