using System;
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
   /// <summary>
   /// A proof of concept xpath 2 parser.
   /// </summary>
   public class XPath2Parser : ParserBase
   {
      protected override ParseContext GetContext()
      {
         return new XPathParseContext();
      }

      protected override void DefineGrammar()
      {
         // Define grammar symbols:
         ParseNode Expression = null, tExprSingle = null, tForExpr = null, tQuantifiedExpr = null,
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

         Root = Define(() => Expression.Eof());

         // Example of using the expression overload of Define.
         Define(() => Expression, () => tExprSingle.FollowedBy(",".FollowedBy(tExprSingle).ZeroOrMore()));

         //tExprSingle = Define(() => tForExpr.Or(tQuantifiedExpr).Or(tIfExpr).Or(tOrExpr).Error("expected 'if' or 'or'..."));
         tExprSingle = Define("ExprSingle", () => tForExpr.Or(tQuantifiedExpr, tIfExpr, tOrExpr));

         //tForExpr = Define(() => tSimpleForClause.FollowedBy("return".FollowedBy(tExprSingle)));
         tForExpr = Define("ForExpr", () => tSimpleForClause.FollowedBy("return", tExprSingle));

         tSimpleForClause = Define("SimpleForClause", () => "for".FollowedBy("\\$", tVarName, "in", tExprSingle, (
               ",".FollowedBy("\\$", tVarName, "in", tExprSingle).ZeroOrMore()
            )));

         tQuantifiedExpr = Define("QualifiedExpr", () => ("some".Or("every")).FollowedBy("\\$", tVarName, "in", tExprSingle,
                ",".FollowedBy("\\$", tVarName, "in", tExprSingle).ZeroOrMore(),
                "satisfies", tExprSingle)
             );

         // todo: comment check (:, could do that in terminal symbol.
         tIfExpr = Define("IfExpr", () => "if".FollowedBy(@"\(", Expression, @"\)", "then", tExprSingle, "else", tExprSingle));

         tOrExpr = Define("OrExpr", () => tAndExpr.FollowedBy("or".FollowedBy(tAndExpr).ZeroOrMore()));

         tAndExpr = Define("AndExpr", () => tComparisonExpr.FollowedBy("and".FollowedBy(tComparisonExpr).ZeroOrMore()));

         tComparisonExpr = Define(() => tRangeExpr.FollowedBy(tValueComp.Or(tGeneralComp, tNodeComp).FollowedBy(tRangeExpr).Optional()));

         tRangeExpr = Define("ComparisonExpr", () => tAdditiveExpr.FollowedBy("to".FollowedBy(tAdditiveExpr).Optional()));
         tAdditiveExpr = Define("AdditiveExpr", () => tMultiplicativeExpr.FollowedBy(@"\-".Or(@"\+").FollowedBy(tMultiplicativeExpr).ZeroOrMore()));
         tMultiplicativeExpr = Define("MultiplicativeExpr", () => tUnionExpr.FollowedBy(@"\*".Or("div").Or("idiv").Or("mod").FollowedBy(tUnionExpr).ZeroOrMore()));
         tUnionExpr = Define("UnionExpr", () => tIntersectExceptExpr.FollowedBy("union".Or(@"\|").FollowedBy(tIntersectExceptExpr).ZeroOrMore()));
         tIntersectExceptExpr = Define("IntersectionExpr", () => tInstanceofExpr.FollowedBy("intersect".Or("except").FollowedBy(tInstanceofExpr).ZeroOrMore()));
         tInstanceofExpr = Define("InstanceOfExpr", () => tTreatExpr.FollowedBy("instance".FollowedBy("of", tSequenceType).Optional())); ;
         tTreatExpr = Define("TreatExpr", () => tCastableExpr.FollowedBy("treat".FollowedBy("as", tSequenceType).Optional()));
         tCastableExpr = Define("CastableExpr",() => tCastExpr.FollowedBy("castable".FollowedBy("as", tSingleType).Optional()));
         tCastExpr = Define("CastExpr", () => tUnaryExpr.FollowedBy("cast".FollowedBy("as", tSingleType).Optional()));
         tUnaryExpr = Define("UnaryExpr", () => @"\+".Or(@"\-").ZeroOrMore().FollowedBy(tValueExpr));
         tValueExpr = Define("ValueExpr", () => tPathExpr);

         tGeneralComp = "=".Or("!=").Or("<").Or("<=").Or(">").Or(">=").SetName("GeneralComp"); // todo: params overload
         tValueComp = "eq".Or("ne").Or("lt").Or("le").Or("gt").Or("ge").SetName("ValueComp");
         tNodeComp = "is".Or("<<").Or(">>").SetName("NodeComp");

         tPathExpr = Define("PathExpr", () => "/".FollowedBy(tRelativePathExpr.Optional()).Or("//".FollowedBy(tRelativePathExpr)).Or(tRelativePathExpr));

         tRelativePathExpr = Define("RelativePathExpr", () => tStepExpr.FollowedBy("/".Or("//").FollowedBy(tStepExpr).ZeroOrMore()));

         tStepExpr = Define("StepExpr", () => tFilterExpr.Or(tAxisStep));

         tAxisStep = Define("AxisStep", () => tReverseStep.Or(tForwardStep).FollowedBy(tPredicateList));

         tForwardStep = Define("ForwardStep", () => tForwardAxis.FollowedBy(tNodeTest).Or(tAbbrevForwardStep));

         tForwardAxis = Define("ForwardAxis", () => "child".FollowedBy("::").Or("descendant".FollowedBy("::")).Or("attribute".FollowedBy("::")).Or("self".FollowedBy("::")).
            Or("descendant\\-or\\-self".FollowedBy("::")).Or("following\\-sibling".FollowedBy("::")).Or("following".FollowedBy("::")).Or("namespace".FollowedBy("::")));

         tReverseStep = Define("ReverseStep", () => tReverseAxis.FollowedBy(tNodeTest).Or(tAbbrevReverseStep));

         // todo: rule not necessary..
         tReverseAxis = Define("ReverseAxis", () => "parent".FollowedBy("::").Or("ancestor".FollowedBy("::")).Or("preceding\\-sibling".FollowedBy("::")).Or("preceding".FollowedBy("::")).Or("ancestor\\-or\\-self".FollowedBy("::")));

         tAbbrevForwardStep = Define("AbbrevForwardStep", () => "@".Optional().FollowedBy(tNodeTest));

         tAbbrevReverseStep = @"\.\.".Terminal().SetName("AbbrevReverseStep"); // > ..

         tNodeTest = Define("NodeTest", () => tKindTest.Or(tNameTest)); // > one of these

         tNameTest = Define("NameTest", () => tQName.Or(tWildCard));

         // tWildCard is handcoded, due to explicit whitespace annotation:
         tWildCard = @"\*".Terminal().SetName("Wildcard"); // TODO
         // idea: introduce a special symbol which matches *any* lookahead not matched elsewhere, say ___ANY___ (as NCName should match anything..)
         //tWildCard = Define(() => "*".Or(new ParseNode() { Optional = true, GetDecisionTerminals = { throw new Exception("too many choices"); }, Parse = 
         //}).Or(new ParseNode() { }));

         tFilterExpr = Define("FilterExpr", () => tPrimaryExpr.FollowedBy(tPredicateList));

         tPredicateList = Define("PredicateList", () => tPredicate.ZeroOrMore());

         tPredicate = Define("Predicate", () => @"\[".FollowedBy(Expression).FollowedBy(@"\]"));

         tPrimaryExpr = Define("PrimaryExpr", () => tLiteral.Or(tVarRef, tParenthesizedExpr, tContextItemExpr, tFunctionCall));

         tLiteral = Define("Literal", () => tNumericLiteral.Or(tStringLiteral));

         tNumericLiteral = Define("NumericLiteral", () => tIntegerLiteral.Or(tDecimalLiteral, tDoubleLiteral));

         // todo :escaping, escaping, escaping: $ for .NET!
         tVarRef = Define(() => @"\$".FollowedBy(tVarName)).SetName("VarRef");

         tParenthesizedExpr = Define("ParenthesizedExpr", () => @"\(".FollowedBy(Expression.Optional()).FollowedBy(@"\)"));

         tContextItemExpr = @"\.".Terminal().SetName("ContextItemExpr");

         tFunctionCall = Define("FunctionCall", () => tQName.FollowedBy(@"\(", tExprSingle.FollowedBy(",".FollowedBy(tExprSingle).ZeroOrMore()).Optional(), @"\)"));

         tSingleType = Define("SingleType", () => tAtomicType.FollowedBy(@"\?".Optional()));

         tSequenceType = Define("SequencyType", () => "empty\\-sequence".FollowedBy(@"\(", @"\)").Or(tItemType.FollowedBy(tOccurrenceIndicator.Optional())));

         tOccurrenceIndicator = @"\?".Or(@"\+").Or(@"\*").SetName("OccurrenceIndicator");

         tItemType = Define("ItemType", () => tKindTest.Or("item".FollowedBy(@"\(", @"\)"), tAtomicType));

         tAtomicType = Define("AtomicType", () => tQName);

         tKindTest = Define("KindTest", () => tDocumentTest.Or(tElementTest, tAttributeTest, tSchemaElementTest, tSchemaAttributeTest, 
            tPITest, tCommentTest, tTextTest, tAnyKindTest));

         tAnyKindTest = Define("AnyKindTest", () => "node".FollowedBy(@"\(", @"\)"));

         tDocumentTest = Define("DocumentTest", () => "document\\-node".FollowedBy(@"\(", tElementTest.Or(tSchemaElementTest).Optional(), @"\)"));

         tTextTest = Define("TextTest", () => "text".FollowedBy(@"\(", @"\)"));

         tCommentTest = Define("CommentTest", () => "comment".FollowedBy(@"\(", @"\)"));

         tPITest = Define("PITest", () => "processing\\-instruction".FollowedBy(@"\(", tNCName.FollowedBy(tStringLiteral).Optional(), @"\)"));

         tAttributeTest = Define("AttributeTest", () => 
            "attribute".FollowedBy(@"\(", tAttribNameOrWildcard.FollowedBy(",".FollowedBy(tTypeName).Optional()).Optional(), @"\)" ));

         tAttribNameOrWildcard = Define("AttributeNameOrWildcard", () => tAttributeName.Or(@"\*"));

         tSchemaAttributeTest = Define("SchemaAttributeTest", () => "schema\\-attribute".FollowedBy(@"\(", tAttributeDeclaration, @"\)"));

         tAttributeDeclaration = Define("AttributeDeclaration", () => tAttributeName);

         tElementTest = Define("ElementTest", () => "element".FollowedBy(@"\(", tElementNameOrWildcard.FollowedBy(",".FollowedBy(tTypeName, @"\?".Optional()).Optional()).Optional() , @"\)"));

         tElementNameOrWildcard = Define("ElementNameOrWildcard", () => tElementName.Or(@"\*"));

         tSchemaElementTest = Define("SchemaElementTest", () => "schema\\-element".FollowedBy(@"\(", tElementDeclaration, @"\)"));

         tElementDeclaration = Define("ElementDeclaration", () => tElementName);

         tAttributeName = Define("AttributeName", () => tQName);

         tElementName = Define("ElementName", () => tQName);

         tTypeName = Define("TypeName", () => tQName);

         // todo: more work, custom parsing
         // the below works, but c.Advance is wrong.
         //tVarName = new SimpleParseNode() {
         //   Label = "VARNAME",
         //   GetDecisionTerminalsDelegate = () => { return new Terminal[] { new GeneralTerminal("VARNAME") }; },
         //   ParseDelegate = c => { c.AdvanceInterleaved(); c.Advance(; c.AdvanceInterleaved(); } // todo: this is incorrec,t but for testing...
         //};

       //  ParseNode tNameStartChar = "[A-Z] | _ | [a-z] | [\xC0-\xD6] | [\xD8-\xF6] | [\xF8-\x2FF] | [\x370-\x37D] | [\x37F-\x1FFF] | [\x200C-\x200D] | [\x2070-\x218F] | [\x2C00-\x2FEF] | [\x3001-\xD7FF] | [\xF900-\xFDCF] | [\xFDF0-\xFFFD]".Terminal();
       //  ParseNode tNameChar = tNameStartChar.Or("\\- | \\. | [0-9] | \xB7 | [\x0300-\x036F] | [\x203F-\x2040]");

         tVarName = Define("VarName", () => tQName);


         // todo
       //  tNCName = "NCNAME".Terminal();
         //tNCName = tNameStartChar.FollowedBy(tNameChar.ZeroOrMore());
         // todo: add [\U00010000-\U000EFFFF]

         String tNameStartChar = "[A-Z] | _ | [a-z] | [\xC0-\xD6] | [\xD8-\xF6] | [\xF8-\x2FF] | [\x370-\x37D] | [\x37F-\x1FFF] | [\x200C-\x200D] | [\x2070-\x218F] | [\x2C00-\x2FEF] | [\x3001-\xD7FF] | [\xF900-\xFDCF] | [\xFDF0-\xFFFD]";
         String tNameChar = "\\- | \\. | [0-9] | \xB7 | [\x0300-\x036F] | [\x203F-\x2040]";

         tNCName = String.Format("({0})({0}|{1})*", tNameStartChar, tNameChar).Terminal().SetName("NCName");

         // Parse NCNAME | NCNAME : NCNAME, rewritten for 1 lookahead.
         //tQName = tNCName.FollowedBy(":".FollowedBy(tNCName).Optional());
         tQName = String.Format("({0})({0}|{1})*(:({0})({0}|{1})*)?", tNameStartChar, tNameChar).Terminal().SetName("QName");
         //tQName = "QNAME".Terminal();
         
         
         // todo: the below QNAME causes assertion failures, which should be proper errors instead.
         //tQName = tNCName.Or(tNCName.FollowedBy(":", tNCName));

         // From the spec, namestartchar:
         
         //tStringLiteral = "'[a-z]".Terminal();
        // tStringLiteral = "(\"(\"\"|[^\"])*\"|'(''|[^'])*')".Terminal().SetName("StringLiteral");
         tStringLiteral = "\"(\"\"|[^\"])*".FollowedBy("\"").Or("'(''|[^'])*".FollowedBy("'")).SetName("StringLiteral");

         //tIntegerLiteral = "Int".Terminal();
         tIntegerLiteral = "[0-9]+".Terminal().SetName("IntegerLiteral");

         //tDecimalLiteral = "Dec".Terminal();
         // ("." Digits) | (Digits "." [0-9]*)
         tDecimalLiteral = @"(\.[0-9]+)|([0-9]+\.[0-9]*)".Terminal().SetName("DecimalLiteral");
         
         //tDoubleLiteral = "E".Terminal();
         // (("." Digits) | (Digits ("." [0-9]*)?)) [eE] [+-]? Digits
         tDoubleLiteral = @"((\.[0-9]+)|([0-9]+(\.[0-9]*)?))[eE][\+\-]?[0-9]+".Terminal().SetName("DoubleLiteral");
      }
   }
}
