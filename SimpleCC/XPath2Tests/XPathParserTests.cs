using System;
using System.Text;
using System.Collections.Generic;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using XPath2.Parser;
using Parser;

namespace XPath2Tests
{
   [TestClass]
   public class XPathParserTests
   {
      ParserBase mParser;

      [TestInitialize]
      public void Init()
      {
         mParser = new XPath2Parser().Build();
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
			   "attribute(QNAME, QNAME)", "attribute(*,QNAME)",
			   "for $VARNAME in QNAME return ( for $VARNAME in QNAME return *)",
			   "for $VARNAME in QNAME return .",
			   "for $VARNAME in QNAME return QNAME",
			   "()", "(  )", " ( ) ",
			   "/../*",
			   "QNAME[*][*]", "QNAME[QNAME][QNAME]", "QNAME[ * ][ QNAME]",
			   "some $VARNAME in QNAME satisfies (some $VARNAME in QNAME satisfies .)",
			   "if (.) then . else .",
			   "if (.,.,. , .) then . else *",
			   "for $VARNAME in (if (.) then . else *) return QNAME",
			   "(( (((((() instance of empty-sequence()) except ()) | ()) idiv ()) + () ) to ()) and ()) or ()",
			   "self::*",
			   "descendant-or-self::*",
			   "following-sibling::*",
			   "following::node()",
			   "namespace::*",
			   "parent::*",
			   "ancestor::*",
			   "preceding-sibling::*",
			   "preceding::*",
			   "ancestor-or-self::*",
			   "/descendant::*",
			   "//child::*",
			   "QNAME = QNAME",
			   "QNAME > QNAME",
			   "QNAME < QNAME",
			   "QNAME >= QNAME",
			   "QNAME <= QNAME",
			   "QNAME != QNAME",
			   "QNAME eq QNAME",
			   "QNAME lt QNAME",
			   "QNAME gt QNAME",
			   "QNAME ge QNAME",
			   "QNAME le QNAME",
			   "QNAME is QNAME",
			   "QNAME ne QNAME",
			   "QNAME >> QNAME",
			   "QNAME << QNAME",
         })
            mParser.Parse(tXPath);
      }
   }
}
