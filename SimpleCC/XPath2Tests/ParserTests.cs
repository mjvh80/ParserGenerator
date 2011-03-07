using System;
using System.Text;
using System.Collections.Generic;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using XPath2.Parser;

namespace XPath2Tests
{
   [TestClass]
   public class ParserTests
   {
      XPath2Parser mParser;

      [TestInitialize]
      public void Init()
      {
         mParser = new XPath2Parser();
      }

      [TestMethod]
      public void GoodXPathsTest()
      {
         // Simple parse, expecting no exceptions.
         foreach (String tXPath in new List<String>()
         {
            "/",
            "//*",
			   "//QNAME",
			   "/*",
			   "/QNAME",
            "QNAME",
            "/QNAME",
			   "/QNAME/QNAME",
			   "/QNAME//QNAME",
			   "/*//*",
			   "$VARNAME",
			   "($VARNAME)",
			   "(QNAME)",
			   ".",
			   "(.)",
			   "QNAME()",
			   "QNAME(QNAME)",
			   "QNAME(QNAME, QNAME)",
			   "QNAME(QNAME,$VARNAME)",
			   "QNAME ( QNAME , $VARNAME  )  ",
			   "   QNAME   ",
			   "   .   ",
			   "node()", "node(  )", "node ( ) ",
			   "text()", " text ()", " text (  )",
			   "comment()", "processing-instruction()",
			   "attribute(QNAME)",
			   "element(QNAME)",
			   "schema-attribute(QNAME)",
			   "schema-element(QNAME)",
			   "element(*)",
			   "attribute(*)",
			   "element(QNAME, QNAME?)", "element(QNAME, QNAME)", "element(*,QNAME?)",
			   "attribute(QNAME, QNAME?)", "attribute(*,QNAME)",
         })
            mParser.Parse(tXPath);
      }
   }
}
