using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Linq.Expressions;
using System.Reflection;
using System.Diagnostics;

namespace SimpleCC
{

   /// <summary>
   /// Implements a BNF parser generator, using our parser generator.
   /// </summary>
   public class BnfParser
   {
      public class BnfParserContext : ParseContext
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
            // todo

            return false;
         }

         public override void AdvanceInterleaved()
         {
            while (AdvanceWhitespace() || AdvanceComment()) ; // todo: can probably do little more efficient...
         }
      }

      protected class ActualBnfParser : ParserBase
      {
         protected ActualParser mActualParser;
         protected Dictionary<String, ParseNode> mProductionLookup;

         public Dictionary<String, ParseNode> ProductionMap { get { return mProductionLookup; } }

         public ActualParser GetBnfDefinedParser() { return mActualParser; } 

         protected override void DefineGrammar()
         {
            mActualParser = new ActualParser();
            mProductionLookup = new Dictionary<string, ParseNode>();


            ParseNode Production = null, Name = null, Definition = null, Sequence = null,
               Literal = null, OccurencyMarker = null, Unit = null, NewLine = null;

            Root = Define(() => Production.OneOrMore().Eof());

            Define(() => Production, () => Name.FollowedBy("::=".FollowedBy(Definition).FollowedBy(";|\r\n|\n"))); // todo: test/make work end of line termination

            // carriage return with optional linefeed, or linefeed
            //NewLine = "\r".FollowedBy("\n".Optional()).Or("\n"); // todo.. needs bit of work with whitespace in context

            Define(() => Definition, () => Sequence.FollowedBy("\\|".FollowedBy(Definition).ZeroOrMore()));

            Define(() => Sequence, () => Unit.FollowedBy(Unit.ZeroOrMore()));

            Define(() => Unit, () => "\\(".FollowedBy(Definition).FollowedBy("\\)").Or(Literal).Or(Name).FollowedBy(OccurencyMarker.Optional()));

            // need string here..
            Define(() => Name, "([a-z]|[A-Z])+");

            Define(() => Literal, "(\"(\"\"|[^\"])*\"|'(''|[^'])*')");

            Define(() => OccurencyMarker, () => "\\?".Or("\\*").Or("\\+"));

            // flatten
            Root.Rewrite(n => new FlatteningVisitor().Flatten((ProductionSyntaxNode)n));

            // compile
            // 

            Root.SetCompiler(n =>
               {
                  for (Int32 i = 0; i < n.Children.Length; i++) // eof is already flattened out
                     n.Children[i].Compile();

                  // Add Eof.
                  mActualParser.AddEofToRoot();

                  return null;
               });
               
            Production.SetCompiler(n =>
            {
               // Given above parser, make a call to Define on it, given a func defined here.

               // Get name value.
               String tProductionName = (String)n.Children[0].Children[0].Value;

               // Assertion.
               Debug.Assert(n.Children[1].Value.ToString() == "::=");

               // Get the definition Expression.
               Expression tDefinition = n.Children[2].Compile();

               // Create our rule and store.
               

               if (tProductionName == "Root")
                  if (mProductionLookup.ContainsKey("Root"))
                     throw new InvalidOperationException("Multiple Root productions defined.");
               
               mProductionLookup.Add(tProductionName, mActualParser.DoRule(tProductionName, Expression.Lambda<Func<ParseNode>>(tDefinition).Compile()));
               if (tProductionName == "Root")
                  mActualParser.SetRoot(mProductionLookup["Root"]);

               return null;
            });

            Name.SetCompiler(n =>
            {
               Func<ParseNode> tInner = () =>
               {
                  String tName = (String)n.Children[0].Value;
                  ParseNode tNode;
                  if (!mProductionLookup.TryGetValue(tName, out tNode))
                     throw new ParseException("Production not found: " + n.Value); // todo improve
                  return tNode;
               };
               Expression<Func<ParseNode>> tExpr = () => tInner(); // expressions with a body cannot be converted to expression, so work around this
               return tExpr.Body;
            });

            Literal.SetCompiler(n =>
            {
               String tValue = (String)n.Children[0].Value;
               if (String.IsNullOrEmpty(tValue))
                  throw new Exception("Invalid empty literal found.");
               Expression<Func<ParseNode>> tExpr = () => StripQuotes(tValue).Terminal();
               return tExpr.Body;
            });

            Definition.SetCompiler(n =>
            {
               Expression tLeft = n.Children[0].Compile();
               for (Int32 i = 2; i < n.Children.Length; i += 2) // skip |
                  tLeft = Expression.Call(GetOrMethod(), tLeft, n.Children[i].Compile());
               return tLeft;
            });

            Sequence.SetCompiler(n =>
            {
               Expression tLeft = n.Children[0].Compile();
               for (Int32 i = 1; i < n.Children.Length; i += 1)
                  tLeft = Expression.Call(GetFollowedByMethod(), tLeft, n.Children[i].Compile());
               return tLeft;
            });

            Unit.SetCompiler(n =>
            {
               Expression tValue = null;
               String tMarker = null;

               Int32 next = 1; // child that is occurency marker

               String tBracket = n.Children[0].Value as String;
               if (tBracket != null && tBracket[0] == '(') // group
               {
                  tValue = n.Children[1].Compile();

                  // Assert.
                  Debug.Assert((String)n.Children[2].Value == ")");

                  next = 3;
               }
               else
                  tValue = n.Children[0].Compile();

               if (next < n.Children.Length) // have occurency marker
                  tMarker = n.Children[next].Children[0].Value as String;

               if (tMarker == null)
                  return tValue;
               else
                  return Expression.Call(GetOccurencyMethod(tMarker), tValue);
            });


         }

         

         protected override ParseContext GetContext()
         {
            return new BnfParserContext();
         }
      }

      // Our actual parser, defined through the BNF.
      protected class ActualParser : ParserBase
      {
         protected override void DefineGrammar()
         {
            // don't do this here
         }

         protected override ParseContext GetContext()
         {
            return new BnfParserContext(); // todo... for now
         }

         public ParseNode DoRule(String productionName, Func<ParseNode> rule)
         {
            return Define(productionName, rule);
         }

         public void SetRoot(ParseNode node)
         {
            Root = node;
         }

         public void AddEofToRoot()
         {
            Root = Root.Eof();
         }
      }

      #region Utilities

      protected static MethodInfo GetOrMethod()
      {
         return typeof(ParserExtensions).GetMethod("Or", BindingFlags.Static | BindingFlags.Public, null, new Type[] { typeof(ParseNode), typeof(ParseNode) }, null);
      }

      protected static MethodInfo GetFollowedByMethod()
      {
         return typeof(ParserExtensions).GetMethod("FollowedBy", BindingFlags.Static | BindingFlags.Public, null, new Type[] { typeof(ParseNode), typeof(ParseNode) }, null);
      }

      protected static String StripQuotes(String str)
      {
         if (str.StartsWith("\"") && str.EndsWith("\""))
            return str.Substring(1, str.Length - 2);
         else
            return str;
      }

      protected static MethodInfo GetOccurencyMethod(String marker)
      {
         switch (marker)
         {
            case "?":
               return typeof(ParserExtensions).GetMethod("Optional", BindingFlags.Static | BindingFlags.Public, null, new Type[] { typeof(ParseNode) }, null);

            case "+":
               return typeof(ParserExtensions).GetMethod("OneOrMore", BindingFlags.Static | BindingFlags.Public, null, new Type[] { typeof(ParseNode) }, null);

            case "*":
               return typeof(ParserExtensions).GetMethod("ZeroOrMore", BindingFlags.Static | BindingFlags.Public, null, new Type[] { typeof(ParseNode) }, null);

            default:
               throw new InvalidOperationException("unknown marker " + marker);
         }
      }

      #endregion

      protected ActualBnfParser mBnfParser;
      protected ParserBase mBnfDefinedParser;

      public BnfParser(String bnf)
      {
         mBnfParser = new ActualBnfParser();

         mBnfParser.Build();
         SyntaxNode tNode = mBnfParser.Parse(bnf);
         tNode.Compile();
         //Expression tResultExpr = tNode.Compile();

         
         //Expression tResultExpr = mBnfParser.Build().Parse(bnf).Compile();
        
         // Run it, and get our built parser.
        // Expression.Lambda<Action>(tResultExpr).Compile()();

         mBnfDefinedParser = mBnfParser.GetBnfDefinedParser().Build();
      }

      // Allows for setting compilers etc.
      public ProductionNode GetProduction(String name)
      {
         return (ProductionNode)mBnfParser.ProductionMap[name];
      }

      public SyntaxNode Parse(String expression)
      {
         return mBnfDefinedParser.Parse(expression);
      }
   }
}
