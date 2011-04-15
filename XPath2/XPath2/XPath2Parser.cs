﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using SimpleCC;

namespace XPath2.Parser
{
   public class XPathParseContext : ParseContext
   {
      // todo: put this in base
      protected Boolean IsWhitespace(Char pChar)
      {
         // todo: properly
         return Char.IsWhiteSpace(pChar);
      }

      protected Boolean IsChar(Char pChar)
      {
         return true; // todo: xml proper
      }

      // Parse whitespace, return true if space was found.
      protected Boolean AdvanceWhitespace()
      {
         Int32 tOldPos = Position;
         while (Position < Expression.Length && IsWhitespace(Expression[Position]))
            Position++;
         return Position != tOldPos;
      }

      // Parse comments with any nesting level, return true if one was found.
      protected Boolean AdvanceComment()
      {
         Int32 tNestingLevel = 1;

         if (Position + 1 < Expression.Length)
         {
            if (Expression[Position] == '(' && Expression[Position + 1] == ':')
            {
               // Ignore Chars
               for (Position = Position + 2; Position < Expression.Length; Position++)
               {
                  // Start of nested comment.
                  Char tChar = Expression[Position];
                  if (tChar == '(' && Position + 1 < Expression.Length && Expression[Position + 1] == ':')
                  {
                     tNestingLevel += 1;
                     Position += 1;
                     continue;
                  }

                  // End of comment.
                  if (tChar == ':' && Position + 1 < Expression.Length && Expression[Position + 1] == ')')
                  {
                     tNestingLevel -= 1;
                     Position += 1;
                     if (tNestingLevel == 0)
                     {
                        Position += 1;
                        return true;
                     }
                     else
                        continue;
                  }

                  if (!IsChar(tChar))
                     throw new ParseException("invalid character found in comment: '{0}' at position {1}", tChar.ToString(), Position.ToString());
               }

               throw new ParseException("unexpected end of input while reading a comment"); // todo: improve with starting info?
            }
         }

         return false;
      }

      public override void AdvanceInterleaved()
      {
         while (AdvanceWhitespace() || AdvanceComment()) ; // todo: can probably do little more efficient...
      }
   }

   // todo: move common stuff into a base class
   public class XPath2Parser : ParserBase
   {
      public XPath2Parser() : base() { }

      protected override ParseContext GetContext()
      {
         return new XPathParseContext();
      }

      protected override void DefineGrammar()
      {
         // Define grammar symbols:
         ParseNode tExpr = null, tExprSingle = null, tForExpr = null, tQuantifiedExpr = null,
            tIfExpr = null, tOrExpr = null, tSimpleForClause = null, tVarName = null, tAndExpr = null, tComparisonExpr = null,
            tRangeExpr = null, tValueComp = null, tGeneralComp = null, tNodeComp = null, tAdditiveExpr = null, tMultiplicativeExpr = null,
            tUnionExpr = null, tIntersectExceptExpr = null, tInstanceofExpr = null, tTreatExpr = null, tCastableExpr = null,
            tCastExpr = null, tUnaryExpr = null, tValueExpr = null, tSequenceType = null, tSingleType = null,
            tPathExpr = null, tForwardAxis = null, tReverseAxis = null, tAbbrevForwardStep = null, tNodeTest = null,
            tKindTest = null, tNameTest = null, tRelativePathExpr = null, tStepExpr = null, tFilterExpr = null, tAxisStep = null,
            tReverseStep = null, tPredicateList = null, tForwardStep = null, tAbbrevReverseStep = null,
            tQName = null, tWildCard = null, tPrimaryExpr = null, tPredicate = null,
            tLiteral = null, tVarRef = null, tParenthesizedExpr = null, tContextItemExpr = null, tFunctionCall = null,
            tNumericLiteral = null, tStringLiteral = null, tIntegerLiteral = null, tDecimalLiteral = null, tDoubleLiteral = null,
            tOccurrenceIndicator = null, tAtomicType = null, tItemType = null,
            tAnyKindTest = null, tDocumentTest = null, tCommentTest = null, tTextTest = null, tPITest = null, tNCName = null,
            tAttributeTest = null, tAttribNameOrWildcard = null, tAttributeName = null,
            tElementTest = null, tElementNameOrWildcard = null, tElementName = null, tTypeName = null,
            tSchemaElementTest = null, tElementDeclaration = null, tSchemaAttributeTest = null, tAttributeDeclaration = null;

         // Define grammar:

         Root = Rule(() => tExpr.EOF());

         tExpr = Rule(() => tExprSingle.FollowedBy(",".FollowedBy(tExprSingle).ZeroOrMore()));

         //tExprSingle = Rule(() => tForExpr.Or(tQuantifiedExpr).Or(tIfExpr).Or(tOrExpr).Error("expected 'if' or 'or'..."));
         tExprSingle = Rule(() => tForExpr.Or(tQuantifiedExpr, tIfExpr, tOrExpr));

         //tForExpr = Rule(() => tSimpleForClause.FollowedBy("return".FollowedBy(tExprSingle)));
         tForExpr = Rule(() => tSimpleForClause.FollowedBy("return", tExprSingle));

         tSimpleForClause = Rule(() => "for".FollowedBy("\\$", tVarName, "in", tExprSingle, (
               ",".FollowedBy("\\$", tVarName, "in", tExprSingle).ZeroOrMore()
            )));

         tQuantifiedExpr = Rule(() => ("some".Or("every")).FollowedBy("\\$", tVarName, "in", tExprSingle,
                ",".FollowedBy("\\$", tVarName, "in", tExprSingle).ZeroOrMore(),
                "satisfies", tExprSingle)
             );

         // todo: comment check (:, could do that in terminal symbol.
         tIfExpr = Rule(() => "if".FollowedBy(@"\(", tExpr, @"\)", "then", tExprSingle, "else", tExprSingle));

         tOrExpr = Rule(() => tAndExpr.FollowedBy("or".FollowedBy(tAndExpr).ZeroOrMore()));

         tAndExpr = Rule(() => tComparisonExpr.FollowedBy("and".FollowedBy(tComparisonExpr).ZeroOrMore()));

         tComparisonExpr = Rule(() => tRangeExpr.FollowedBy(tValueComp.Or(tGeneralComp, tNodeComp).FollowedBy(tRangeExpr).Optional()));

         tRangeExpr = Rule(() => tAdditiveExpr.FollowedBy("to".FollowedBy(tAdditiveExpr).Optional()));
         tAdditiveExpr = Rule(() => tMultiplicativeExpr.FollowedBy(@"\-".Or(@"\+").FollowedBy(tMultiplicativeExpr).ZeroOrMore()));
         tMultiplicativeExpr = Rule(() => tUnionExpr.FollowedBy(@"\*".Or("div").Or("idiv").Or("mod").FollowedBy(tUnionExpr).ZeroOrMore()));
         tUnionExpr = Rule(() => tIntersectExceptExpr.FollowedBy("union".Or(@"\|").FollowedBy(tIntersectExceptExpr).ZeroOrMore()));
         tIntersectExceptExpr = Rule(() => tInstanceofExpr.FollowedBy("intersect".Or("except").FollowedBy(tInstanceofExpr).ZeroOrMore()));
         tInstanceofExpr = Rule(() => tTreatExpr.FollowedBy("instance".FollowedBy("of", tSequenceType).Optional())); ;
         tTreatExpr = Rule(() => tCastableExpr.FollowedBy("treat".FollowedBy("as", tSequenceType).Optional()));
         tCastableExpr = Rule(() => tCastExpr.FollowedBy("castable".FollowedBy("as", tSingleType).Optional()));
         tCastExpr = Rule(() => tUnaryExpr.FollowedBy("cast".FollowedBy("as", tSingleType).Optional()));
         tUnaryExpr = Rule(() => @"\+".Or(@"\-").ZeroOrMore().FollowedBy(tValueExpr));
         tValueExpr = Rule(() => tPathExpr);

         tGeneralComp = "=".Or("!=").Or("<").Or("<=").Or(">").Or(">="); // todo: params overload
         tValueComp = "eq".Or("ne").Or("lt").Or("le").Or("gt").Or("ge");
         tNodeComp = "is".Or("<<").Or(">>");

         tPathExpr = Rule(() => "/".FollowedBy(tRelativePathExpr.Optional()).Or("//".FollowedBy(tRelativePathExpr)).Or(tRelativePathExpr));

         tRelativePathExpr = Rule(() => tStepExpr.FollowedBy("/".Or("//").FollowedBy(tStepExpr).ZeroOrMore()));

         tStepExpr = Rule(() => tFilterExpr.Or(tAxisStep));

         tAxisStep = Rule(() => tReverseStep.Or(tForwardStep).FollowedBy(tPredicateList));

         tForwardStep = Rule(() => tForwardAxis.FollowedBy(tNodeTest).Or(tAbbrevForwardStep));

         tForwardAxis = Rule(() => "child".FollowedBy("::").Or("descendant".FollowedBy("::")).Or("attribute".FollowedBy("::")).Or("self".FollowedBy("::")).
            Or("descendant\\-or\\-self".FollowedBy("::")).Or("following\\-sibling".FollowedBy("::")).Or("following".FollowedBy("::")).Or("namespace".FollowedBy("::")));

         tReverseStep = Rule(() => tReverseAxis.FollowedBy(tNodeTest).Or(tAbbrevReverseStep));

         // todo: rule not necessary..
         tReverseAxis = Rule(() => "parent".FollowedBy("::").Or("ancestor".FollowedBy("::")).Or("preceding\\-sibling".FollowedBy("::")).Or("preceding".FollowedBy("::")).Or("ancestor\\-or\\-self".FollowedBy("::")));

         tAbbrevForwardStep = Rule(() => "@".Optional().FollowedBy(tNodeTest));

         tAbbrevReverseStep = @"\.\.".Terminal(); // > ..

         tNodeTest = Rule(() => tKindTest.Or(tNameTest)); // > one of these

         tNameTest = Rule(() => tQName.Or(tWildCard));

         // tWildCard is handcoded, due to explicit whitespace annotation:
         tWildCard = @"\*".Terminal(); // TODO
         // idea: introduce a special symbol which matches *any* lookahead not matched elsewhere, say ___ANY___ (as NCName should match anything..)
         //tWildCard = Rule(() => "*".Or(new ParseNode() { Optional = true, GetDecisionTerminals = { throw new Exception("too many choices"); }, Parse = 
         //}).Or(new ParseNode() { }));

         tFilterExpr = Rule(() => tPrimaryExpr.FollowedBy(tPredicateList));

         tPredicateList = Rule(() => tPredicate.ZeroOrMore());

         tPredicate = Rule(() => @"\[".FollowedBy(tExpr).FollowedBy(@"\]"));

         tPrimaryExpr = Rule(() => tLiteral.Or(tVarRef, tParenthesizedExpr, tContextItemExpr, tFunctionCall));

         tLiteral = Rule(() => tNumericLiteral.Or(tStringLiteral));

         tNumericLiteral = Rule(() => tIntegerLiteral.Or(tDecimalLiteral, tDoubleLiteral));

         // todo :escaping, escaping, escaping: $ for .NET!
         tVarRef = Rule(() => @"\$".FollowedBy(tVarName));

         tParenthesizedExpr = Rule(() => @"\(".FollowedBy(tExpr.Optional()).FollowedBy(@"\)"));

         tContextItemExpr = @"\.".Terminal();

         tFunctionCall = Rule(() => tQName.FollowedBy(@"\(", tExprSingle.FollowedBy(",".FollowedBy(tExprSingle).ZeroOrMore()).Optional(), @"\)"));

         tSingleType = Rule(() => tAtomicType.FollowedBy(@"\?".Optional()));

         tSequenceType = Rule(() => "empty\\-sequence".FollowedBy(@"\(", @"\)").Or(tItemType.FollowedBy(tOccurrenceIndicator.Optional())));

         tOccurrenceIndicator = @"\?".Or(@"\+").Or(@"\*");

         tItemType = Rule(() => tKindTest.Or("item".FollowedBy(@"\(", @"\)"), tAtomicType));

         tAtomicType = Rule(() => tQName);

         tKindTest = Rule(() => tDocumentTest.Or(tElementTest, tAttributeTest, tSchemaElementTest, tSchemaAttributeTest, 
            tPITest, tCommentTest, tTextTest, tAnyKindTest));

         tAnyKindTest = "node".FollowedBy(@"\(", @"\)");

         tDocumentTest = Rule(() => "document\\-node".FollowedBy(@"\(", tElementTest.Or(tSchemaElementTest).Optional(), @"\)"));

         tTextTest = "text".FollowedBy(@"\(", @"\)");

         tCommentTest = "comment".FollowedBy(@"\(", @"\)");

         tPITest = Rule(() => "processing\\-instruction".FollowedBy(@"\(", tNCName.FollowedBy(tStringLiteral).Optional(), @"\)"));

         tAttributeTest = Rule(() => 
            "attribute".FollowedBy(@"\(", tAttribNameOrWildcard.FollowedBy(",".FollowedBy(tTypeName).Optional()).Optional(), @"\)" ));

         tAttribNameOrWildcard = Rule(() => tAttributeName.Or(@"\*"));

         tSchemaAttributeTest = Rule(() => "schema\\-attribute".FollowedBy(@"\(", tAttributeDeclaration, @"\)"));

         tAttributeDeclaration = Rule(() => tAttributeName);

         tElementTest = Rule(() => "element".FollowedBy(@"\(", tElementNameOrWildcard.FollowedBy(",".FollowedBy(tTypeName, @"\?".Optional()).Optional()).Optional() , @"\)"));

         tElementNameOrWildcard = Rule(() => tElementName.Or(@"\*"));

         tSchemaElementTest = Rule(() => "schema\\-element".FollowedBy(@"\(", tElementDeclaration, @"\)"));

         tElementDeclaration = Rule(() => tElementName);

         tAttributeName = Rule(() => tQName);

         tElementName = Rule(() => tQName);

         tTypeName = Rule(() => tQName);

         // todo: more work, custom parsing
         // the below works, but c.Advance is wrong.
         //tVarName = new SimpleParseNode() {
         //   Label = "VARNAME",
         //   GetDecisionTerminalsDelegate = () => { return new Terminal[] { new GeneralTerminal("VARNAME") }; },
         //   ParseDelegate = c => { c.AdvanceInterleaved(); c.Advance(; c.AdvanceInterleaved(); } // todo: this is incorrec,t but for testing...
         //};

         ParseNode tNameStartChar = "[A-Z] | _ | [a-z] | [\xC0-\xD6] | [\xD8-\xF6] | [\xF8-\x2FF] | [\x370-\x37D] | [\x37F-\x1FFF] | [\x200C-\x200D] | [\x2070-\x218F] | [\x2C00-\x2FEF] | [\x3001-\xD7FF] | [\xF900-\xFDCF] | [\xFDF0-\xFFFD]".Terminal();
         ParseNode tNameChar = tNameStartChar.Or("\\- | \\. | [0-9] | \xB7 | [\x0300-\x036F] | [\x203F-\x2040]");

         tVarName = "VARNAME".Terminal();


         // todo
         tNCName = "NCNAME".Terminal();
         // todo: namechar includes the range [\x10000-\xEFFFF] -> how?
       //  tNCName = tNameStartChar.FollowedBy(tNameChar.ZeroOrMore());

         // Parse NCNAME | NCNAME : NCNAME, rewritten for 1 lookahead.
         //tQName = tNCName.FollowedBy(":".FollowedBy(tNCName).Optional());
         tQName = "QNAME".Terminal();
         
         
         // todo: the below QNAME causes assertion failures, which should be proper errors instead.
         //tQName = tNCName.Or(tNCName.FollowedBy(":", tNCName));

         // From the spec, namestartchar:
         
         //tStringLiteral = "'[a-z]".Terminal();
         tStringLiteral = "(\"(\"\"|[^\"])*\"|'(''|[^'])*')".Terminal();
         
         //tIntegerLiteral = "Int".Terminal();
         tIntegerLiteral = "[0-9]+".Terminal();

         //tDecimalLiteral = "Dec".Terminal();
         // ("." Digits) | (Digits "." [0-9]*)
         tDecimalLiteral = @"(\.[0-9]+)|([0-9]+\.[0-9]*)".Terminal();
         
         //tDoubleLiteral = "E".Terminal();
         // (("." Digits) | (Digits ("." [0-9]*)?)) [eE] [+-]? Digits
         tDoubleLiteral = @"((\.[0-9]+)|([0-9]+(\.[0-9]*)?))[eE][\+\-]?[0-9]+".Terminal();
      }
   }
}
