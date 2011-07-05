using System;
using System.Text;
using System.Collections.Generic;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using XPath2.Parser;
using SimpleCC;

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

      public static String[] ValidXPathExpressions =
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
         "Q()",
         "a()",
         "\xC1()",
         "QNAME()",
         "foo:bar()",
         "f:b()",
         "foo:b()",
         "f:bar()",
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
         "element(QNAME, QNAME)", // FUNCTION call
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
         "2",
         "2.23"
      };

      public static String[] InvalidXPathExpressions = new[] {
         "element(QNAME, QNAME?)", // this is a element test, NOT a function call, element test not allowed like this
         "element(*,QNAME?)",
         "element(QNAME?)"
      };

      [TestMethod]
      public void GoodXPathsTest()
      {
         // Simple parse, expecting no exceptions.
         foreach (String tXPath in ValidXPathExpressions)
            try
            {
               mParser.Parse(tXPath);
            }
            catch (Exception e)
            {
               throw new Exception("Error parsing: " + tXPath, e);
            }
      }

      [TestMethod]
      public void BadXPathsTest()
      {
         foreach(String tXPath in InvalidXPathExpressions)
            try
            {
               mParser.Parse(tXPath);
               Assert.Fail(String.Format("XPath {0} is invalid, but parsed.", tXPath));
            }
            catch
            {
               // OK
            }
      }
   }
}
