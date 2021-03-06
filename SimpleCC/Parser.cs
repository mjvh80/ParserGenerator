﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using SimpleRegexIntersector;
using System.Diagnostics;
using System.Linq.Expressions;

// Open issues / todo:
// 1. Escaping, regex syntax often clashes with DSL syntax (it seems) requiring a lot of escaping in our definitions, e.g.
//    see xpath code. This isn't as clean as it could be, can we do better?
// 2. / vs //.., / should accept an Eof terminal as lookahead, how to do with SimpleRegex?
// 3. We must review the API, in particular, look at accessibility (public, internal etc. etc.).
// 4. We should provide with defining Interleaving as a grammar in such a way that ParseContext can still be used.
// 5. Review calls to AdvanceInterleaved. Ensure more tests are written to test interleaving.
// 6. Look at "issubsetof" instead of "shares prefix".
// 7. General review of code.
// 8. Settings, ie parse defaults greedily or not. Ability to switch off validity checks (ie faster parser generation) etc. etc.

// 2 ideas:
// 1. we can cache the current lookahead, advance takes a terminal as follows:
//  - narrow down to a terminal that makes the choice
//  - any subsequent narrowing simply returns this terminal
//  - the terminal is not cleared until Advance(terminal) is called.
//  - how to deal with 1 lookahead? cache 2?
//
// 2. Eof should become a special terminal, EofTerminal.
// 3. Having introduced more general terminals gives us a problem where our current algorithm fails and lookahead is insufficient.
//    eg /a/ vs /attribute::* (or to select a node //attribute/...) etc.
//    To solve this we need to introduce a general terminal, this terminal is a regular expression.
//    In order to be able to deal with lookahead, we need to be able to tell if 2 regular expressions have an intersection.
//    This is needed as we can not simply produce a general lookahead table, as we may have conflicts, eg 'a' '::' | 'b' '::', in this case
//    no lookahead table is needed as the prefixes do not intersect. Proposed algorithm:
//  a. for each current terminal, see if it's a match (we could do this by generating a single regex, marking each with a named group).
//  b. if 0 matches: parse error
//     if 1 match: ok, choose
//     if >= 2 matches: lookahead needed, so perform parsing of lookahead terminals (regexes).
//       if >= 2 match < invalid operation, should have been detected by not allowing an intersection
//       if 1 match: ok, got our lookahead -> parse
//       if 0 matches: parse error
// 4. Look at M grammar for how to do projection much much better.


namespace SimpleCC
{
   public class ParseException : Exception
   {
      public ParseException(String msg) : base(msg) { }
      public ParseException(String format, params Object[] args) : base(String.Format(format, args)) { }
      public ParseException(String msg, Exception inner) : base(msg, inner) { }
      public ParseException(Exception inner, String format, params Object[] args) : base(String.Format(format, args), inner) { }
   }

   internal static class _Utils
   {
      public static Boolean All<T>(IEnumerable<T> list, Func<T, T, Boolean> predicate)
      {
         IEnumerator<T> tEnumerator = list.GetEnumerator();
         for(Int32 i = 1; tEnumerator.MoveNext(); i++)
            foreach (T tOther in list.Skip(i))
            {
               if (!predicate(tEnumerator.Current, tOther))
                  return false;
            }

         return true;
      }
   }

   public static class ParserExtensions
   {
      public static ParseNode Rewrite(this ParseNode pNode, Func<SyntaxNode, SyntaxNode> pRewriter)
      {
         pNode.AddRewriter(pRewriter);
         return pNode;
      }

      public static ParseNode SetCompiler(this ParseNode pNode, Func<SyntaxNode, Expression> pCompiler)
      {
         // hmm..
         pNode.Rewrite(n =>
            {
               n.Compiler = pCompiler;
               return n;
            });

         return pNode;
      }

      public static ParseNode FlattenProduction(this ParseNode pNode)
      {
         if (!(pNode is ProductionNode))
            throw new Exception("Flattening must be done on a production node.");

         return pNode.Rewrite(n => new FlatteningVisitor().Flatten((ProductionSyntaxNode)n));
      }


      //// Older code as idea. What?
      //// as we're needing lookahead, we can assume that decision terminals were needed, ie we can call GetDecisionTe..
      //public static ParseNode Lookahead(this ParseNode node, String lookaheadTerminal)

      // Idea: Not, can we do this? Is it useful?
      //public static ParseNode Not(this ParseNode node, ParseNode notNode)
      
      // todo: overload to deal better with escaping?
      public static ParseNode Terminal(this String terminalExpression)
      {
         if (terminalExpression == null)
            throw new ParseException("Terminal: expression is null");

         return new TerminalParseNode(terminalExpression);
      }

      /// <summary>
      /// Same as .FollowedBy(new EofNode()).
      /// </summary>
      /// <param name="node"></param>
      /// <returns></returns>
      public static ParseNode Eof(this ParseNode node)
      {
         return node.FollowedBy(new EofNode());
      }

      public static ParseNode Or(this String terminalStr, String otherTerminal)
      {
         return Or(terminalStr.Terminal(), otherTerminal.Terminal());
      }

      public static ParseNode Or(this String terminalStr, ParseNode otherTerminal)
      {
         return Or(terminalStr.Terminal(), otherTerminal);
      }

      public static ParseNode Or(this ParseNode node, String terminalStr)
      {
         return Or(node, terminalStr.Terminal());
      }

      public static ParseNode Or(this ParseNode node, ParseNode otherNode)
      {
         return new OrParseNode(new ParseNode[] { node, otherNode });
      }

      public static ParseNode Or(this ParseNode action, params ParseNode[] otherActions)
      {
         ParseNode[] tArgs = new ParseNode[otherActions.Length + 1];
         tArgs[0] = action;
         Array.Copy(otherActions, 0, tArgs, 1, otherActions.Length);
         return new OrParseNode(tArgs);
      }

      public static ParseNode Or(this ParseNode action, params Object[] otherActions)
      {
         // ok, lazy.
         return Or(action, (from n in otherActions
                            select
                               n is String ? Terminal((String)n) : (ParseNode)n).ToArray());
      }

      public static ParseNode FollowedBy(this ParseNode node, ParseNode otherNode)
      {
         if (node == null)
            throw new ParseException("FollowedBy: left node is null, this could indicate a production is not defined");
         if (otherNode == null)
            throw new ParseException("FollowedBy: right node is null, this could indicate a production is not defined");

         return new FollowedByParseNode(node, otherNode);
      }

      public static ParseNode FollowedBy(this String terminalStr, ParseNode node)
      {
         return terminalStr.Terminal().FollowedBy(node);
      }

      public static ParseNode FollowedBy(this ParseNode node, ParseNode otherNode, params Object[] others)
      {
         ParseNode tFirstNode = node.FollowedBy(otherNode);
         foreach (Object tNode in others)
            if (tNode is String)
               tFirstNode = tFirstNode.FollowedBy(Terminal((String)tNode));
            else
               tFirstNode = tFirstNode.FollowedBy((ParseNode)tNode);
         return tFirstNode;
      }

      public static ParseNode FollowedBy(this String node, String terminalStr, params Object[] others)
      {
         return FollowedBy(Terminal(node), Terminal(terminalStr), others);
      }

      public static ParseNode FollowedBy(this String terminalStr, ParseNode node, params Object[] others)
      {
         return FollowedBy(Terminal(terminalStr), node, others);
      }

      public static ParseNode FollowedBy(this ParseNode node, String terminalStr, params Object[] others)
      {
         return FollowedBy(node, Terminal(terminalStr), others);
      }

      private static ParseNode ParseN(this ParseNode node, Int32 requiredCount, Int32 upperBound)
      {
         return new OptionalParseNode(node, requiredCount, upperBound);
      }

      public static ParseNode Optional(this String terminalStr)
      {
         return Optional(terminalStr.Terminal());
      }

      public static ParseNode Optional(this ParseNode node)
      {
         return ParseN(node, 0, 1);
      }

      public static ParseNode ZeroOrMore(this ParseNode node)
      {
         return ParseN(node, 0, Int32.MaxValue);
      }

      public static ParseNode ZeroOrMore(this String terminalStr)
      {
         return ZeroOrMore(terminalStr.Terminal());
      }

      public static ParseNode OneOrMore(this ParseNode node)
      {
         return ParseN(node, 1, Int32.MaxValue);
      }

      public static ParseNode OneOrMore(this String terminalStr)
      {
         return OneOrMore(terminalStr.Terminal());
      }
   }

   public class ParseContext
   {
      public ParseContext()
      {
         Position = 0;
      }

      public ParseException CreateParseException(String msg)
      {
         return CreateParseException(msg, null);
      }

      public virtual ParseException CreateParseException(String format, params Object[] args)
      {
         String tActualMsg = String.Format(format, args);
         return new ParseException("Error parsing production {0} around position {1}: {2}", CurrentlyParsingProduction.Name, Position, tActualMsg);
      }

      public String Expression;

      /// <summary>
      /// Production currently being parsed, currently used for error handling.
      /// </summary>
      public ProductionNode CurrentlyParsingProduction { get; set; }

      public Int32 Position { get; set; } // todo: check usages, external

      /// <summary>
      /// Advance, raising an exception if the found terminal does not match the string found.
      /// </summary>
      /// <param name="str"></param>
      /// <returns></returns>
      protected void Advance(String str) 
      {
         if (str == null)
            throw new ArgumentNullException("str");

         AdvanceInterleaved();

         Int32 tStartingPosition = Position;
         Int32 i;
         for (i = 0; i < str.Length && Position < Expression.Length && str[i] == Expression[Position]; i++, Position++) ;

         if (i != str.Length)
         {
            Position = tStartingPosition; // reset for correct position in exception
            throw CreateParseException("Expected '{0}'", str);
            //throw new ParseException("Expected '{0}' around position {1}", str, tStartingPosition.ToString());
         }

         // todo: advance interleaved again?

      }

      public String Advance(Terminal term)
      {
         AdvanceInterleaved();

         String tResult = term.AdvanceTerminal(this);

         //if (term is StringTerminal)
         //   Advance(((StringTerminal)term).TerminalString);
         //else
         //{
         //   Char tChar = Expression[Position];
         //   Position += 1;
         //   if (!term.Matches(tChar, 0))
         //      throw new InvalidOperationException("expected single char terminal match - got " + tChar);
         //}

         AdvanceInterleaved();

         return tResult;
      }
    
      //  public String Peek()
      
      /// <summary>
      /// Default implementation ignores space as defined by Char.IsWhitespace. If you wish to ignore comments
      /// you must implement this method.
      /// todo: we should allow one to define interleaving just like regular syntax.
      /// todo: implement using Char.IsWhitespace
      /// </summary>
      public virtual void AdvanceInterleaved() { }
   }

   public class EofNode : ParseNode
   {
      public EofNode()
      {
         this.mName = "«eof»";
      }

      public override Terminal[] GetDecisionTerminals()
      {
         Debug.Assert(Mode == PrimeMode.Primed);
         throw new InvalidOperationException("Eof decision terminals needed");
      }

      public override SyntaxNode Parse(ParseContext c)
      {
         Debug.Assert(Mode == PrimeMode.Primed);

         c.AdvanceInterleaved();

         if (c.Position != c.Expression.Length)
            throw new ParseException("Expected end of input, but stopped at {0}.", c.Position.ToString());

         return Rewrite(new EofSyntaxNode()
         {
            Children = null,
            Name = null,
         });
      }

      public override Boolean LookaheadTerminals(Terminal s, HashSet<Terminal> l)
      {
         throw new InvalidOperationException("Lookahead terminals for Eof needed.");
      }

      public override void Accept(ParseNodeVisitor visitor)
      {
         visitor.Visit(this);
      }

      internal override StringBuilder ToString(StringBuilder builder, HashSet<ParseNode> seenNodes)
      {
         builder.Append(Name);
         return base.ToString(builder, seenNodes);
      }
   }

   // value => choice made
   public class OptionalParseNode : ParseNode
   {
      public ParseNode InnerNode { get { return mInnerNode; } }

      protected ParseNode mInnerNode;
      protected ParseTable<ParseNode> mParseTable;
      protected Int32 mUpperBound, mRequiredCount;

      public OptionalParseNode(ParseNode node, Int32 requiredCount, Int32 upperBound)
      {
         if (node == null)
            throw new ArgumentNullException("OptionalParseNode: inner node is null");

         this.mUpperBound = upperBound;
         this.mRequiredCount = requiredCount;
         this.mInnerNode = node;
         mParseTable = new ParseTable<ParseNode>();
      }

      internal override Boolean DerivesExclusivelyTo(ParseNode pNode)
      {
         Debug.Assert(IsPrimed());

         if (pNode == this)
            return true;

         if (!this.Optional)
            return mInnerNode.DerivesExclusivelyTo(pNode);
         
         return false;
      }

      internal override void VerifyTree()
      {
         base.VerifyTree();
         mInnerNode.VerifyTree();
      }

      internal override void InitRegexBuilder(SimpleRegexBuilder builder, ParserBase parser)
      {
         mInnerNode.InitRegexBuilder(builder, parser);
      }

      protected override Boolean PrimeInternal(ParserBase parser)
      {
         return mInnerNode.Prime(parser);

         // used to build parsetable here, not necessary
      }

      internal override ParseNode.PrimeMode Mode
      {
         get
         {
            // An optional node is primed if it's underlying node is.
            return mInnerNode == null ? PrimeMode.None : mInnerNode.Mode;
         }
         set
         {
            base.Mode = value;
         }
      }

      public override Terminal[] GetDecisionTerminals()
      {
         Debug.Assert(Mode == PrimeMode.Primed);

         return mInnerNode.GetDecisionTerminals();
      }

      public override SyntaxNode Parse(ParseContext c)
      {
         //Debug.Assert(Mode == PrimeMode.Primed);
         Debug.Assert(mInnerNode.IsPrimed());

         List<SyntaxNode> tResult = new List<SyntaxNode>();

         // Parse minimal count.
         for (Int32 i = 0; i < mRequiredCount; i++)
            tResult.Add(mInnerNode.Parse(c)); // simply go in

     //    Debug.Assert(this.IsPrimed()); // this would be an error...

         // Parse optional max count.
         Int32 tCountLeft = (mUpperBound == Int32.MaxValue ? Int32.MaxValue : (mUpperBound - mRequiredCount));
         for (Int32 i = 0; i < tCountLeft && tCountLeft > 0; i++)
         {
            c.AdvanceInterleaved(); // todo: should not be needed.. check

            foreach (Terminal tTerminal in mInnerNode.GetDecisionTerminals()) // > todo cache
               if (tTerminal.CanAdvance(c))
               {
                  tResult.Add(mInnerNode.Parse(c));
                  goto next;
               }

            break; // we're done

         next: ;
         }

         return Rewrite(new OptionalSyntaxNode()
         {
            Children = tResult.ToArray(),
         });
      }

      public override Boolean LookaheadTerminals(Terminal s, HashSet<Terminal> termList)
      {
         Debug.Assert(Mode == PrimeMode.Primed);

         if (mInnerNode.LookaheadTerminals(s, termList) && mRequiredCount > 0)
            return true; // required, should stop
         else
            return false;
      }

      public override Boolean ParsesTerminal(Terminal s)
      {
         Debug.Assert(Mode == PrimeMode.Primed);

         return mInnerNode.ParsesTerminal(s);
      }

      public override Boolean Optional
      {
         get
         {
            // Note: we used to check if the node was primed here, but this is not really correct.
            // Some situations require us to know if a node is optional, even if not primed.
            // Not requiring this simplifies FollowedBy, and priming no longer needs a stack to keep track
            // of nodes still needing priming.
          
            return mRequiredCount == 0;
         }
      }

      public override void Accept(ParseNodeVisitor visitor)
      {
         visitor.Visit(this);
      }

      internal override StringBuilder ToString(StringBuilder builder, HashSet<ParseNode> seenNodes)
      {
         if (HasToStringVisited(builder, seenNodes))
            return builder;

         builder.Append('(');
         mInnerNode.ToString(builder, seenNodes);
         builder.Append(')');
         if (mRequiredCount == 0 && mUpperBound == 1)
            builder.Append('?');
         else if (mRequiredCount == 0)
            builder.Append('*');
         else
            builder.Append('+'); // for now

         return base.ToString(builder, seenNodes);
      }
   }

   public class TerminalParseNode : ParseNode
   {
      protected Boolean mShouldInitBuilder = true;

      protected Char[] GetLetters()
      {
         return mInitialCompiledRegex.Letters();
      }

      protected IEnumerable<RangeRegex> GetRanges()
      {
         return mInitialCompiledRegex.GetClonedRanges();
      }

      protected SimpleRegex mInitialCompiledRegex;
      protected String mTerminalExpression;

      protected Terminal mTerminal;

      public TerminalParseNode(String terminalExpr)
      {
         mTerminalExpression = terminalExpr;
         mInitialCompiledRegex = SimpleRegexBuilder.Default.Parse(terminalExpr);
      }

      //public TerminalParseNode(GeneralTerminal pTerminal)
      //{
      //   mTerminal = pTerminal;
      //}

      public override Boolean ParsesTerminal(Terminal s)
      {
         Debug.Assert(this.Mode == PrimeMode.Primed);

         return s.SemanticEquals(mTerminal);
      }

      internal override void InitRegexBuilder(SimpleRegexBuilder builder, ParserBase parser)
      {
         if (mShouldInitBuilder)
         {
            foreach (Char tLetter in GetLetters())
               builder.AddLetter(tLetter);

            foreach (RangeRegex tRange in GetRanges())
               builder.AddRange(tRange);

            mShouldInitBuilder = false;
         }
      }

      protected override Boolean PrimeInternal(ParserBase parser)
      {
         Debug.Assert(parser.RegexBuilder.IsBuilt);
         Debug.Assert(!mShouldInitBuilder);

         // Enough info to build our terminal.
         mTerminal = new Terminal(mTerminalExpression, parser.RegexBuilder);
         return base.PrimeInternal(parser);
      }

      public override SyntaxNode Parse(ParseContext c)
      {
         System.Diagnostics.Debug.Assert(this.Mode == PrimeMode.Primed);
         String tResult = c.Advance(mTerminal);

         return Rewrite(new ValueSyntaxNode()
         {
            Children = null,
            Value = tResult,
         });
      }

      public override Terminal[] GetDecisionTerminals()
      {
         System.Diagnostics.Debug.Assert(this.Mode == PrimeMode.Primed);
         return new[] { mTerminal };
      }

      public override Boolean LookaheadTerminals(Terminal s, HashSet<Terminal> l)
      {
         return this.ParsesTerminal(s);
      }

      public override void Accept(ParseNodeVisitor visitor)
      {
         visitor.Visit(this);
      }

      internal override StringBuilder ToString(StringBuilder builder, HashSet<ParseNode> seenNodes)
      {
         if (mName == null)
            return builder.Append('"').Append(mTerminal.ToString()).Append('"');
         else
            return builder.Append(mName);
      }
   }

   public class FollowedByParseNode : ParseNode
   {
      public ParseNode First { get { return node; } }
      public ParseNode Next { get { return otherNode; } }

      protected ParseNode node, otherNode;

      public FollowedByParseNode(ParseNode node, ParseNode otherNode)
      {
         if (node == null || otherNode == null)
            throw new ArgumentNullException("one node argument is null"); 

         this.node = node;
         this.otherNode = otherNode;
      }

      public override void Accept(ParseNodeVisitor visitor)
      {
         visitor.Visit(this);
      }

      internal override Boolean DerivesExclusivelyTo(ParseNode pNode)
      {
         if (pNode == this)
            return true;
         // todo: asserts
         if (!node.Optional)
            return node.DerivesExclusivelyTo(pNode);
         
         if (otherNode.Optional)
            return false; // both optional

         return otherNode.DerivesExclusivelyTo(pNode);
      }

      internal override void VerifyTree()
      {
         Debug.Assert(IsPrimed());

         if (mNodeVerified)
            return;

         base.VerifyTree();

         if (node.DerivesExclusivelyTo(this))
            throw new ParseException("derives to self");

         if (otherNode.DerivesExclusivelyTo(this))
            throw new ParseException("derives to self");

         node.VerifyTree();
         otherNode.VerifyTree();
      }

      internal override void InitRegexBuilder(SimpleRegexBuilder builder, ParserBase parser)
      {
         node.InitRegexBuilder(builder, parser);
         otherNode.InitRegexBuilder(builder, parser);
      }

      protected override Boolean PrimeInternal(ParserBase parser)
      {
         Boolean tNodePrimed = node.Prime(parser);

         Boolean tOtherNodePrimed = otherNode.Prime(parser);

         // In this case we need the other node for decision terminals, thus we could not proceed.
         if (tNodePrimed && node.Optional && !tOtherNodePrimed)
            return false;
 
         // Return true only if the other node is primed, or is priming. If it would not be priming, it may never get primed, so don't proceed in that case.
         return tNodePrimed && (tOtherNodePrimed || otherNode.IsPriming());
      }

      /// <summary>
      /// A FollowedByNode is primed if it's necessary conditions for priming are met, in other words, calling prime is not necessary as
      /// it's internal nodes are in a state necessary to continue (so things like decision terminals are available because the node and otherNode are
      /// in a state where this is possible.
      /// </summary>
      internal override ParseNode.PrimeMode Mode
      {
         get
         {
            // 1 . check if our dependencies are primed. If not we must call prime to ensure either one gets primed.
            if (node.Mode == PrimeMode.None || otherNode.Mode == PrimeMode.None)
               return PrimeMode.None; // prime must be called to set either priming

            if (node.IsPrimed())
            {
               if (node.Optional)
                  // otherNode is needed for decision terminals, so we're only primed if it is primed
                  return otherNode.IsPrimed() ? PrimeMode.Primed : PrimeMode.Priming;
               else
                  // By our first if, othernode must be priming. So we are ok to return primed here: this node can parse now.
                  return PrimeMode.Primed;
            }

            return PrimeMode.Priming;
         }
         set
         {
            //base.Mode = value;
         }
      }

      public override Terminal[] GetDecisionTerminals()
      {
         Debug.Assert(Mode == PrimeMode.Primed);

         if (node.Optional)
         {
            Terminal[] tNodeTerms = node.DoGetDecisionTerminals();
            Terminal[] tOtherNodeTerms = otherNode.DoGetDecisionTerminals();

            Terminal[] tResult = new Terminal[tNodeTerms.Length + tOtherNodeTerms.Length];
            Array.Copy(tNodeTerms, tResult, tNodeTerms.Length);
            Array.Copy(tOtherNodeTerms, 0, tResult, tNodeTerms.Length, tOtherNodeTerms.Length);
            return tResult;
         }
         else
         {
            return node.DoGetDecisionTerminals();
         }
      }

      public override Boolean ParsesTerminal(Terminal pTerminal)
      {
         Debug.Assert(Mode == PrimeMode.Primed);

         // A B parses A only if B is optional. If A is optional, then we test B.
         return (otherNode.Optional && node.ParsesTerminal(pTerminal)) || (node.Optional && otherNode.ParsesTerminal(pTerminal));
      }

      public override SyntaxNode Parse(ParseContext c)
      {
         Debug.Assert(Mode == PrimeMode.Primed);

         SyntaxNode tLeft = node.Parse(c);
         SyntaxNode tRight = otherNode.Parse(c);

         return Rewrite(new SequenceSyntaxNode()
         {
            Children = new [] { tLeft, tRight },
         });
      }

      public override Boolean LookaheadTerminals(Terminal s, HashSet<Terminal> termList)
      {
         Debug.Assert(Mode == PrimeMode.Primed);

         // Given a terminal s, must determine a lookahead in A B.
         // If s in A (exclusive) then lookahead in B.
         // Else, if A optional then lookahead in B.
         // Else: no lookahead here.

         if (node.ParsesTerminal(s)) // > terminals in othernode are lookahead terminals
         {
            // node may not exclusively parse the terminal, so check it for lookahead
            // instead, do this below
            //node.LookaheadTerminals(s, termList); // result irrelevant

            // current terminal matches, so lookahead is found in othernode
            foreach (Terminal tOtherTerminal in otherNode.DoGetDecisionTerminals())
               termList.Add(tOtherTerminal);

            return !otherNode.Optional; // if optional, we may not be done
         }
         
         // Else: lookahead found in node, if optional should continue looking in otherNode.

         /* else */
         if (node.LookaheadTerminals(s, termList)) // && !node.Optional)
         {
            // Note: if the node is non-optional, it's lookahead method must return true by definition.
            Debug.Assert(!node.Optional, "non-optional node should return an exhaustive lookahead list");
            return true; // exhausted
         }
         else
            return otherNode.LookaheadTerminals(s, termList) && !otherNode.Optional;
      }

      public override Boolean Optional
      {
         get
         {
            return node.Optional && otherNode.Optional;
         }
      }

      internal override StringBuilder ToString(StringBuilder builder, HashSet<ParseNode> seenNodes)
      {
         if (HasToStringVisited(builder, seenNodes))
            return builder;

         node.ToString(builder, seenNodes);
         builder.Append(' ');
         otherNode.ToString(builder, seenNodes);

         return base.ToString(builder, seenNodes);
      }
   }

   // value => choice made out of options
   public class OrParseNode : ParseNode
   {
      public IEnumerable<ParseNode> Nodes
      {
         get
         {
            foreach (ParseNode node in mActions)
               yield return node;
         }
      }

      protected ParseTable<ParseNode> mParseTable;
      protected ParseNode[] mActions;

      public OrParseNode(ParseNode[] actions)   
      {
         if (actions == null)
            throw new ArgumentNullException("actions");

         if (actions.Length < 2)
            throw new ArgumentException("Or must have at least 2 arguments");

         if (actions.Any(n => n == null))
            throw new ArgumentException("Or: one parse node in argument array is null");

         mParseTable = new ParseTable<ParseNode>();
         this.mActions = FlattenActions(actions);
      }

      /// <summary>
      /// Simple optimization: Or(a, Or(b, c)) = Or(a, b, c ...)
      /// In general, without this function, our Ors redo some work (perhaps less with proper caching).
      /// In such cases, Or(a, Or(b, c)) may determine that we should proceed en route Or(b, c). This is a waste
      /// as we can already know whether we should choose b or c.
      /// </summary>
      /// <param name="otherActions"></param>
      /// <returns></returns>
      protected ParseNode[] FlattenActions(ParseNode[] otherActions)
      {
         List<ParseNode> tResult = new List<ParseNode>(otherActions.Length);
         foreach (ParseNode tNode in otherActions)
            if (tNode == null)
               throw new ParseException("Or: one of the Or argument nodes is null");
            else if (tNode is OrParseNode)
            {
               OrParseNode tOrNode = (OrParseNode)tNode;
               tResult.AddRange(tOrNode.mActions); // note: already flattened!
            }
            else
               tResult.Add(tNode);

         return tResult.ToArray();
      }

      internal override Boolean DerivesExclusivelyTo(ParseNode pNode)
      {
         Debug.Assert(this.IsPrimed());

         if (pNode == this)
            return true;

         Boolean tAllDerive = true;
         foreach (ParseNode tOtherNode in mActions)
            if (tAllDerive)
               tAllDerive = !tOtherNode.Optional && tOtherNode.DerivesExclusivelyTo(pNode);
            else
               break;

         return tAllDerive;
      }

      internal override void VerifyTree()
      {
         if (mNodeVerified)
            return;

         base.VerifyTree(); // also prevenst reentry, marks node verified..

         if (mActions.All(n => n.DerivesExclusivelyTo(this)))
            throw new ParseException("node derives exclusively to self"); // todo: msg

         foreach (ParseNode tOtherNode in mActions)
            tOtherNode.VerifyTree();
      }

      internal override void InitRegexBuilder(SimpleRegexBuilder builder, ParserBase parser)
      {
         foreach (ParseNode tNode in this.Nodes)
            tNode.InitRegexBuilder(builder, parser);
      }

      protected override Boolean PrimeInternal(ParserBase parser)
      {
         Boolean tAllPrimed = true;

         foreach (ParseNode tNode in mActions)
         {
            if (tAllPrimed)
               tAllPrimed = tNode.Prime(parser);
            else
               tNode.Prime(parser); // always prime
         }

         if (!tAllPrimed)
            return false;

         Debug.Assert(!IsPrimed(), "or node already primed");

         // With all nodes primed, build parse table.

         // todo: should we check if terminals of one node can conflict? - yes..
         // .. as we otherwise pick a first one we can advance resulting in probable undeterministic behaviour.

         Dictionary<Terminal, HashSet<ParseNode>> tConflictMap = new Dictionary<Terminal, HashSet<ParseNode>>();
        

#if false
         // Collect all terminals.
   //      List<Terminal> tTerminalList = mActions.SelectMany(tOtherNode => tOtherNode.DoGetDecisionTerminals()).ToList();

         // var tTerminalList = from tNode in mActions
         var tTerminalList = mActions.SelectMany(n => from t in n.DoGetDecisionTerminals() select new { Node = n, Terminal = t });

         using (var tTerminalEnumerator = tTerminalList.GetEnumerator())
            for (Int32 i = 1; tTerminalEnumerator.MoveNext(); i++)
            {
               Boolean tNoConflict = true;

               foreach (var tOther in tTerminalList.Skip(i)) // don't do the conflict test twice (through symmetry), it's a little intensive
               {
                  if (tTerminalEnumerator.Current.Terminal.ConflictsWith(tOther.Terminal))
                  {
                     if (tTerminalEnumerator.Current.Node == tOther.Node)
                        ;// throw new ParseException("");

                     tAddConflict(tTerminalEnumerator.Current.Terminal, tTerminalEnumerator.Current.Node);
                     tAddConflict(tOther.Terminal, tOther.Node);

                     tNoConflict = false;
                  }
               }

               if (tNoConflict)
                  mParseTable.AddTerminal(tTerminalEnumerator.Current.Terminal, tTerminalEnumerator.Current.Node);
            }
#endif

#if false
         // var tTerminalList = from tNode in mActions

         for (Int32 i = 0; i < mActions.Length; i++)
         {
            ParseNode tCurrentNode = mActions[i];

            // Check this node has no terminals conflicting.
            Terminal[] tCurrentDecisionTerminals = tCurrentNode.DoGetDecisionTerminals();
            for (Int32 j = 0; j < tCurrentDecisionTerminals.Length; j++)
               for (Int32 k = j + 1; k < tCurrentDecisionTerminals.Length; k++)
                  if (tCurrentDecisionTerminals[j].Equals(tCurrentDecisionTerminals[k]))
                     throw new ParseException("foobar");

            for(Int32 j = i + 1; j < mActions.Length; j++)
            {
               ParseNode tOther = mActions[j];
               Terminal[] tOtherTerminals = tOther.DoGetDecisionTerminals();

               // Test all terminals for conflicts.
               foreach (var tCurrentTerminal in tCurrentDecisionTerminals)
               {
                  Boolean tNoConflict = true;

                  foreach (var tOtherTerminal in tOtherTerminals)
                     if (tCurrentTerminal.ConflictsWith(tOtherTerminal))
                     {
                        tAddConflict(tCurrentTerminal, tCurrentNode);
                        tAddConflict(tOtherTerminal, tOther);

                        tNoConflict = false;
                     }

                  if (tNoConflict)
                  {
                     if (mParseTable.ContainsTerminal(tCurrentTerminal))
                     {
                        tAddConflict(tCurrentTerminal, tCurrentNode);
                        tAddConflict(tCurrentTerminal, mParseTable[tCurrentTerminal]);
                     }
                     else
                        mParseTable.AddTerminal(tCurrentTerminal, tCurrentNode);
                  }
               }
            }
         }
#endif

#if true
         foreach (ParseNode tOtherNode in mActions)
            foreach (Terminal tTerminal in tOtherNode.DoGetDecisionTerminals())
            {

               if (mParseTable.ContainsTerminal(tTerminal))
               {
                  ParseNode tExistingNode = mParseTable[tTerminal];

                  _AddConflict(tConflictMap, tTerminal, tExistingNode, tOtherNode);
               }
               else
                  mParseTable.AddTerminal(tTerminal, tOtherNode);
            }

         // Test for shared prefixes. This is used to distinguish for example in the xpath case / from //, as the regex for / will match on input //.
         // todo: this should be "is subset of"? Our case belowe is too general, as we require lookahead for a case such as "abcd" and "abce", when this is not needed.
         using(IEnumerator<Terminal> tTerminals = mParseTable.Terminals.GetEnumerator())
            for (Int32 i = 1; tTerminals.MoveNext(); i++)
               foreach (Terminal tOtherTerminal in mParseTable.Terminals.Skip(i)) // don't do the conflict test twice (through symmetry), it's a little intensive
                  if (tTerminals.Current.ConflictsWith(tOtherTerminal))
                  {
                     _AddConflict(tConflictMap, tTerminals.Current, mParseTable[tTerminals.Current]);
                     _AddConflict(tConflictMap, tOtherTerminal, mParseTable[tOtherTerminal]);
                  }
#endif
         
         LookaheadParseNode tLookaheadNode = new LookaheadParseNode(tConflictMap); // new LookaheadParseNode(tConflictingTerminals, tParseTable);

         // Replace with lookahead node.
         foreach (Terminal tConflictingTerminal in tConflictMap.Keys)
            mParseTable[tConflictingTerminal] = tLookaheadNode;

         return true; // we are primed
      }

      private static void _AddConflict (Dictionary<Terminal, HashSet<ParseNode>> tConflictMap, Terminal terminal, ParseNode node, params ParseNode[] otherNodes)
      {
         HashSet<ParseNode> tSet;
         if (!tConflictMap.TryGetValue(terminal, out tSet))
         {
            tSet = new HashSet<ParseNode>();
            tConflictMap.Add(terminal, tSet);
         }

         //if (tSet.Contains(n))
         //   throw new ParseException("Node {0} contains two conflicting terminals.", n.Name);

         tSet.Add(node);
         foreach (ParseNode tNode in otherNodes)
            tSet.Add(tNode);
      }

      public override Terminal[] GetDecisionTerminals()
      {
         Debug.Assert(this.Mode == PrimeMode.Primed);
         return mParseTable.Terminals.ToArray(); // todo array?
      }

      public override Boolean ParsesTerminal(Terminal s)
      {
         Debug.Assert(this.Mode == PrimeMode.Primed);
         return mParseTable.ContainsTerminal(s) && mParseTable[s].ParsesTerminal(s);
      }

      public override SyntaxNode Parse(ParseContext c)
      {
         // todo: there is a way we can be much (?) more efficient here.
         // instead of looping through the parsetable, we can lift off of .nets regex engine:
         // create a regex (( terminal 1) | ( terminal 2) | ... ), using named groups to distinguish the regexes.
         c.AdvanceInterleaved(); // todo: must integrate into walker EVEN NECESSARY NOW?
         foreach(KeyValuePair<Terminal, ParseNode> tPair in mParseTable.KeyValues)
            if (tPair.Key.CanAdvance(c))
            {
               SyntaxNode tResult = tPair.Value.Parse(c);
               return Rewrite(new ChoiceSyntaxNode()
               {
                  Children = new[] { tResult },
               }
               );
            }

         RaiseExpectedTerminals(c, mParseTable.Terminals);
         throw new InvalidOperationException(); // damn compiler
      }

      public override Boolean LookaheadTerminals(Terminal s, HashSet<Terminal> l)
      {
         Debug.Assert(Mode == PrimeMode.Primed);

         if (mParseTable.ContainsTerminal(s))
         {
            return mParseTable[s].LookaheadTerminals(s, l);
         }
         return false;
      }

      public override Boolean Optional
      {
         get
         {
            return mActions.Any(n => n.Optional);
         }
      }

      public override void Accept(ParseNodeVisitor visitor)
      {
         visitor.Visit(this);
      }

      internal override StringBuilder ToString(StringBuilder builder, HashSet<ParseNode> seenNodes)
      {
         if (HasToStringVisited(builder, seenNodes))
            return builder;

         mActions[0].ToString(builder, seenNodes);
         for(Int32 i = 1; i < mActions.Length; i++)
         {
            builder.Append(" | ");
            mActions[i].ToString(builder, seenNodes);
         }

         return base.ToString(builder, seenNodes);
      }
   }

   public class SimpleParseNode : ParseNode
   {
      public Func<ParseContext, SyntaxNode> ParseDelegate;

      public override SyntaxNode Parse(ParseContext c)
      {
         return Rewrite(ParseDelegate(c));
      }

      public Func<Terminal[]> GetDecisionTerminalsDelegate;

      public override Terminal[] GetDecisionTerminals()
      {
         return GetDecisionTerminalsDelegate();
      }

      public override void Accept(ParseNodeVisitor visitor)
      {
         visitor.Visit(this);
      }

      internal override StringBuilder ToString(StringBuilder builder, HashSet<ParseNode> seenNodes)
      {
         builder.Append(Name);
         return base.ToString(builder, seenNodes);
      }
   }

   public class ParseNodeVisitor
   {
      public virtual void Visit(OrParseNode orNode) {}
      public virtual void Visit(FollowedByParseNode followNode) { }
      public virtual void Visit(TerminalParseNode terminalNode) { }
      public virtual void Visit(LookaheadParseNode lookaheadNode) { }
      public virtual void Visit(OptionalParseNode optionalNode) { }
      public virtual void Visit(EofNode eofNode) { }
      public virtual void Visit(SimpleParseNode simpleNode) { }
      public virtual void Visit(ProductionNode prodNode) { }
   }

   // todo: this won't work with looping!
   // looping caused by productionnodes, keep hashset?
   public class TreeWalkingVisitor : ParseNodeVisitor
   {
      public override void Visit(FollowedByParseNode followNode)
      {
         base.Visit(followNode);
         followNode.First.Accept(this);
         followNode.Next.Accept(this);
      }

      public override void Visit(OptionalParseNode optionalNode)
      {
         base.Visit(optionalNode);
         optionalNode.InnerNode.Accept(this);
      }

      public override void Visit(OrParseNode orNode)
      {
         base.Visit(orNode);
         foreach (ParseNode tNode in orNode.Nodes)
            tNode.Accept(this);
      }

      public override void Visit(ProductionNode prodNode)
      {
         base.Visit(prodNode);
         prodNode.StartNode.Accept(this);
      }
   }

   // implements a visitor to collect all chars / ranges in all regexes so we can normalize
   //public class RegexBuilderVisitor : TreeWalkingVisitor
   //{
   //   protected HashSet<Char> mAlphabet = new HashSet<Char>();
   //   protected HashSet<RangeRegex> mRanges = new HashSet<RangeRegex>();

   //   public override void Visit(TerminalParseNode terminalNode)
   //   {
   //      foreach (Char tChar in terminalNode.GetLetters())
   //         mAlphabet.Add(tChar);
   //      foreach (RangeRegex tRange in terminalNode.GetRanges())
   //         mRanges.Add(tRange);

   //      base.Visit(terminalNode);
   //   }

   //   public override void Visit(ProductionNode prodNode)
   //   {
   //      prodNode.EnsureNode(); 
   //      base.Visit(prodNode);
   //   }

   //   public SimpleRegexBuilder CreateBuilder()
   //   {
   //      Debug.Assert(mAlphabet.Count > 0); // that'd be weird wouldn't it
   //      return new SimpleRegexBuilder(mAlphabet, mRanges);
   //   }
   //}

   public abstract class ParseNode
   {
      protected String mName;
      public virtual String Name { 
         get { return mName ?? "«unnamed»"; }
         set { mName = value;}
      }

      public virtual ParseNode SetName(String name)
      {
         this.Name = name;
         return this;
      }

      public virtual Boolean Optional
      {
         get { return false; }
      }
      
      public abstract Terminal[] GetDecisionTerminals();

      public abstract SyntaxNode Parse(ParseContext c);

      // return true if route is exhausted, false otherwise
      public virtual Boolean LookaheadTerminals(Terminal s, HashSet<Terminal> l)
      {
         return false;
      }

      public abstract void Accept(ParseNodeVisitor visitor);

      //public Func<Stack<ParseNode>, Boolean> Primer;
      protected virtual Boolean PrimeInternal(ParserBase parser)
      {
         return true; // default primer
      }

      // Return true if we ended up where we started, exclusively.
      // If other paths were possible this returns false. If any path is optional, this would not be exclusive as 
      // we could match the empty string instead.
      // If other paths are possible, this should return false.
      internal virtual Boolean DerivesExclusivelyTo(ParseNode pNode)
      {
         if (pNode == this)
            return true;

         return false;
      }

      protected Boolean mNodeVerified = false;

      // Method for running post priming verification of the tree.
      // Currently used to detect an infinite parser.
      internal virtual void VerifyTree()
      {
         mNodeVerified = true;

         if (!this.IsPrimed())
            throw new ParseException("unprimed node detected"); // todo: better error?
      }

      internal virtual void InitRegexBuilder(SimpleRegexBuilder builder, ParserBase parser) { }

      internal enum PrimeMode { None = 0, Priming, Primed };
      
      //protected PrimeMode Mode = PrimeMode.None;
      // Some nodes are primed based on their underlying node, such as optional nodes.
      internal virtual PrimeMode Mode { get; set; }
     
      public Boolean Prime(ParserBase parser) 
      {
         switch (Mode)
         {
            case PrimeMode.None:
               Mode = PrimeMode.Priming;
               if (PrimeInternal(parser))
                  Mode = PrimeMode.Primed;
               else
                  Mode = PrimeMode.None;
               
               break;
         }

         return Mode == PrimeMode.Primed;
      }

      public Terminal[] DoGetDecisionTerminals() // todo: rename?
      {
         switch (Mode)
         {
            case PrimeMode.None:
               throw new InvalidOperationException("node not primed");

            case PrimeMode.Priming:
               throw new ParseException("A node depends on the decision terminals of itself.");

            default:
               return GetDecisionTerminals();
         }
      }

      public virtual Boolean ParsesTerminal(Terminal pTerminal)
      {
         Debug.Assert(Mode == PrimeMode.Primed);
         return false;
      }

      public Boolean IsPriming() { return Mode == PrimeMode.Priming; }
      public Boolean IsPrimed() { return Mode == PrimeMode.Primed; }

      protected static void RaiseExpectedTerminals(ParseContext ctx, IEnumerable<Terminal> terminals)
      {
         // todo: add positional info
         throw ctx.CreateParseException("Expected one of '{0}', found '{1}'", String.Join(", ", terminals), String.Join<Terminal>(" or ", terminals)); // todo
      }

      protected SyntaxNode Rewrite(SyntaxNode pNode)
      {
         return mRewriter == null ? pNode : mRewriter(pNode);
      }
      
      protected Func<SyntaxNode, SyntaxNode> mRewriter;

      internal ParseNode AddRewriter(Func<SyntaxNode, SyntaxNode> pRewriter)
      {
         mRewriter += pRewriter;
         return this;
      }

      protected virtual Boolean HasToStringVisited(StringBuilder builder, HashSet<ParseNode> seenNodes)
      {
         if (seenNodes.Contains(this))
         {
            // something better?
            builder.Append("recursing(").Append(this.Name).Append(')');
            return true;
         }
         return false;
      }

      internal virtual StringBuilder ToString(StringBuilder builder, HashSet<ParseNode> seenNodes)
      {
         seenNodes.Add(this);
         return builder;
      }

      public override string ToString()
      {
         return ToString(new StringBuilder(), new HashSet<ParseNode>()).ToString();
      }
   }

   // only intended for insertion
   public class LookaheadParseNode : ParseNode
   {
      protected Dictionary<Terminal, HashSet<ParseNode>> mConflictMap;
      protected Dictionary<Terminal, List<LookaheadInfo>> mLookaheadMap;


      // todo: must make this more general, ability to add nodes if conflicts are more general...
      /*
       * Three terminals A B C may not all intersect, we may have that A and C intersect, and B and C but not A and C.
       * At the moment I'll stick to the simple case where A B C are all assumed to intersect. We can generalize this
       * by looking at intersection groups, and determining which group to go for during parsing (say A&C) and then deciding
       * on the lookahead.
       * */
      public LookaheadParseNode(Dictionary<Terminal, HashSet<ParseNode>> pConflictMap) // can pass builder here if necessary
      {
         mConflictMap = pConflictMap;

         this.Prime(null); // todo: make bit nicer, just mark as primed.. not applicable here..

         // For lookahead we need a map GeneralTerminal -> list of Pair(LookaheadTerminals, node)
         // ie choose node if the lookahead terminal were to match after terminal
         mLookaheadMap = new Dictionary<Terminal, List<LookaheadInfo>>();
         foreach (var tItem in pConflictMap)
         {
            foreach (ParseNode tNode in tItem.Value)
            {
               HashSet<Terminal> tLookaheadTerminals = new HashSet<Terminal>();
               tNode.LookaheadTerminals(tItem.Key, tLookaheadTerminals); // get lookahead

               Boolean tIsDefaultRoute = false;

               // Look to see if there should be a default route.
               // If the node parses the terminal, it should be selected if no other lookahead routes are available.
               // eg for our xpath case: attribute::,attribute() and attribute (as an element name, nametest). If none
               // of the lookaheads match, ie :: and () then we should select the nametest route. In case of multiple default
               // routes, we will greedily pick the one that parses the most.
               if (tNode.ParsesTerminal(tItem.Key)) // todo: cache result?
                  tIsDefaultRoute = true;
                  //tLookaheadTerminals.Add(new DefaultTerminal());
               
               if (tIsDefaultRoute || tLookaheadTerminals.Count > 0)
               {
                  List<LookaheadInfo> tList;
                  if (!mLookaheadMap.TryGetValue(tItem.Key, out tList))
                  {
                     tList = new List<LookaheadInfo>();
                     mLookaheadMap.Add(tItem.Key, tList);
                  }

                  tList.Add(new LookaheadInfo() { Terminals = tLookaheadTerminals, Node = tNode, IsDefaultRoute = tIsDefaultRoute });
                  //tLookaheadMap.Add(tItem.Key, new Pair<List<GeneralTerminal>, ParseNode>() { Left = tLookaheadTerminals, Right = tNode });
               }
               else
                  throw new Exception("no lookahead terminals");
            }
         }

         // 3. Determine if our lookahead map is unique, every route is unique.
         // This does a lot of work.


         List<Terminal> tDefaultTerminals = new List<Terminal>();
         Int32 tCount = 0;

         foreach(var tEntry in mLookaheadMap)
         {
            tCount += 1;

            // Check that the routes for this terminal are unique..
            // Test that for each two lookahead terminals (non-default), there is no intersection. If there were, given an input matching that intersection
            // we would not be able to choose the correct parsenode to proceed with, as both would have matching lookahead.
            // Note that we do not check for intersection for terminals under a single node. If there is a conflict there, it should have been caught by that node itself,
            // when it primes and decides two terminals can conflict.
            var tEnumerator = tEntry.Value.GetEnumerator();
            for (Int32 i = 1; tEnumerator.MoveNext(); i++)
               foreach (var tOtherValue in tEntry.Value.Skip(i))
               {
                  // For each terminal of the current node, check against all other terminals of the other nodes.
                  foreach (Terminal tTerminal in tEnumerator.Current.Terminals)
                     foreach (Terminal tOtherTerminal in tOtherValue.Terminals)
                        if (tTerminal.SimpleRegex.Intersects(tOtherTerminal.SimpleRegex))
                           throw new ParseException("Inconclusive lookahead, terminals {0} and {1} share possible common input.", tTerminal, tOtherTerminal);
               }

            //IEnumerable<GeneralTerminal> tTerminalList = tEntry.Value.SelectMany(p => p.Left).Where(t => !(t is DefaultTerminal)).Cast<GeneralTerminal>();
            //IEnumerator<GeneralTerminal> tEnumerator = tTerminalList.GetEnumerator();
            //for (Int32 i = 1; tEnumerator.MoveNext(); i++)
            //   foreach (GeneralTerminal tOther in tTerminalList.Skip(i))
            //   {
            //      if (tEnumerator.Current.SimpleRegex.Intersects(tOther.SimpleRegex))
            //         throw new ParseException("Inconclusive lookahead, terminals {0} and {1} share possible common input.", tEnumerator.Current, tOther);
            //   }


            foreach (var tInfo in tEntry.Value)
            {
               // Check if the list contains a default entry.
               //if (tInfo.Terminals.Any(t => t is DefaultTerminal))
               if (tInfo.IsDefaultRoute)
               {
                  if (tDefaultTerminals.Count > 0) // see if we have a conflict
                  {
                     foreach (Terminal tOtherDefaultTerm in tDefaultTerminals)
                        if (tEntry.Key.SimpleRegex.Intersects(tOtherDefaultTerm.SimpleRegex))  // sharing a prefix is OK here: we'll handle this in the parser, they should just not intersect
                           throw new ParseException("2 terminals with defaults clashing"); // todo: obviously improve the msg here
                  }
                  
                  tDefaultTerminals.Add(tEntry.Key);
               }

               // NOTE: all regexes are already normalized, the result is also normalized
               SimpleRegex tLeftRegex = SimpleRegex.Sequence(tEntry.Key.SimpleRegex,
                  SimpleRegex.Choice(from t in tInfo.Terminals select t.SimpleRegex));

               foreach (var tOtherEntry in mLookaheadMap.Skip(tCount)) // start past entry
               {
                  foreach (var tOtherPair in tOtherEntry.Value)
                  {
                     SimpleRegex tRightRegex = SimpleRegex.Sequence(tOtherEntry.Key.SimpleRegex,
                        SimpleRegex.Choice(from t in tOtherPair.Terminals select t.SimpleRegex));

                     if (tLeftRegex.Intersects(tRightRegex))
                        throw new ParseException("inconclusive lookahead");
                  }
               }
            }          
         }
      }

      public override Terminal[] GetDecisionTerminals()
      {
         throw new InvalidOperationException(); // todo: return terminalStr? I think it makes no sense to ask for dec. terminals here.
      }

      // NOTE: this has been added, not entirely sure if correct!!
      public override Boolean LookaheadTerminals(Terminal s, HashSet<Terminal> l)
      {
         // todo: correct?
         Boolean tResult = true;
         foreach (ParseNode tNode in mConflictMap[s])
            tResult = tResult && tNode.LookaheadTerminals(s, l);
         return tResult;
      }

      internal override void InitRegexBuilder(SimpleRegexBuilder builder, ParserBase parser)
      {
         throw new InvalidOperationException();
      }

      // A lookahead node parser the terminal if the key is equal (ie it parses) and the actual lookahead selector
      // is the default route.
      public override Boolean ParsesTerminal(Terminal pTerminal)
      {
         foreach(var tPair in mLookaheadMap)
            if (tPair.Key.SemanticEquals(pTerminal))
               foreach(var tInfo in tPair.Value)
                  if (tInfo.IsDefaultRoute)
                  //if (tInfo.Terminals.Any(t => t is DefaultTerminal))
                     return true;

         return false;
      }

      public override SyntaxNode Parse(ParseContext c)
      {
         // todo: build large, single .NET regex to do our heavy lifting
         // ie (?<production>...) | (?<other_production>...) etc.
         // todo: every time we get here, we already know which terminal advanced, can we use this?

         Int32 tPosition = c.Position;

         Int32 tDefaultPosition = -1; // position of last default match, parser will be greedy (todo: make setting? greedy or not)
         ParseNode tDefaultNode = null; // chosen if no lookahead matches.

         //foreach (GeneralTerminal tTerminal in mLookaheadMap.Keys)
         foreach (KeyValuePair<Terminal, List<LookaheadInfo>> tTerminalListPair in mLookaheadMap)
         {
            //if (tTerminal.CanAdvance(c))
            //{
            //   tTerminal.AdvanceTerminal(c); // takes care of interleaving
            if (tTerminalListPair.Key.OptAdvance(c))
            {
               // Choose a lookahead terminal, and then proceed.
               foreach (var tInfo in tTerminalListPair.Value)
               {
                  // Try to parse.
                  foreach (Terminal tLookaheadTerminal in tInfo.Terminals)
                     if (tLookaheadTerminal.CanAdvance(c))
                     {
                        // NOW we're in business.
                        c.Position = tPosition; // reset for actual parsing (todo: this is a shame)
                        return tInfo.Node.Parse(c); // gtfo
                     }

                  // Else, if we accept a default route, record how far we got for greedy parsing.
                  if (tInfo.IsDefaultRoute)
                  {
                     if (c.Position > tDefaultPosition) // greedily found next default route -> todo setting
                     {
                        tDefaultNode = tInfo.Node;
                        tDefaultPosition = c.Position;
                     }
                  }
               }

               // Failure, reset position, try next terminal.
               c.Position = tPosition;
            }
         }

         // If there was no acceptable route, move into the default route, if there is one.
         if (tDefaultNode != null)
         {
            return tDefaultNode.Parse(c);
         }

         throw new ParseException("parse error"); // todo: improve error
      }

      public override void Accept(ParseNodeVisitor visitor)
      {
         visitor.Visit(this);
      }

      internal override StringBuilder ToString(StringBuilder builder, HashSet<ParseNode> seenNodes)
      {
         throw new InvalidOperationException();
      }
   }

   public class LookaheadInfo
   {
      public HashSet<Terminal> Terminals; 
      public ParseNode Node;
      public Boolean IsDefaultRoute = false;

      public override Boolean Equals(object obj)
      {
         LookaheadInfo tOther = obj as LookaheadInfo;
         return tOther != null && Terminals.Equals(tOther.Terminals) && Node.Equals(tOther.Node) && IsDefaultRoute == tOther.IsDefaultRoute;
      }

      public override int GetHashCode()
      {
         return Terminals.GetHashCode() ^ Node.GetHashCode() ^ IsDefaultRoute.GetHashCode();
      }
   }

   public class ProductionNode : ParseNode
   {
      public ParseNode StartNode { get { return mNode; } }

      protected ParseNode mNode;
      protected Func<ParseNode> mNodeGenerator;

      public ProductionNode(Func<ParseNode> nodeGenerator) : this(null, nodeGenerator)
      { }

      // note on names: a name does not *have* to be unique, just like naming in general.
      public ProductionNode(String name, Func<ParseNode> nodeGenerator)
      {
         mNodeGenerator = nodeGenerator;
         mName = name;
      }

      internal override Boolean DerivesExclusivelyTo(ParseNode pNode)
      {
         return this == pNode || mNode.DerivesExclusivelyTo(pNode);
      }

      internal override void  VerifyTree()
      {
         base.VerifyTree();
         mNode.VerifyTree();
      }

      //internal void EnsureNode()
      //{
      //   if (mNode == null)
      //      mNode = mNodeGenerator();
      //}

      protected Boolean mRegexBuilderInit = false;

      internal override void InitRegexBuilder(SimpleRegexBuilder builder, ParserBase parser)
      {
         if (mRegexBuilderInit)
            return;

         if (mNode == null)
            mNode = mNodeGenerator();

         if (mNode == null)
            throw new ParseException("Missing production");

         mRegexBuilderInit = true; // don't reenter
         mNode.InitRegexBuilder(builder, parser);
      }

      protected override Boolean PrimeInternal(ParserBase parser)
      {
         // prevents things like : A -> B and B -> A
         // todo: it would be MUCH nicer if we could say which rule etc. Could we do this? Reflection (horrible?)?
        
         if (mNode != null && mNode.IsPriming())
            throw new ParseException("Circular grammar: production node {0} depends on node {1} in a circular way.", this.Name, mNode.Name);

         //if (mNode == null)
         //   mNode = mNodeGenerator();

         //if (mNode == null)
         //   throw new ParseException("missing production");

         Boolean tResult = mNode.Prime(parser);

         return tResult;
      }

      public override Boolean Optional
      {
         get
         {
            if (mNode == null)
               throw new InvalidOperationException("node not primed");

            return mNode.Optional;
         }
      }

      public override Terminal[] GetDecisionTerminals()
      {
         Debug.Assert(Mode == PrimeMode.Primed);

         return mNode.GetDecisionTerminals();
      }

      public override SyntaxNode Parse(ParseContext ctx)
      {
         Debug.Assert(Mode == PrimeMode.Primed);

         ctx.CurrentlyParsingProduction = this;

         SyntaxNode tResult = mNode.Parse(ctx);

         return Rewrite(new ProductionSyntaxNode()
         {
            Production = this,
            Children = new[] { tResult },
            Name = mName,
         } );
      }

      public override Boolean ParsesTerminal(Terminal pTerminal)
      {
         Debug.Assert(Mode == PrimeMode.Primed);
         return mNode.ParsesTerminal(pTerminal);
      }

      public override Boolean LookaheadTerminals(Terminal s, HashSet<Terminal> l)
      {
         Debug.Assert(Mode == PrimeMode.Primed);
         return mNode.LookaheadTerminals(s, l);
      }

      public override void Accept(ParseNodeVisitor visitor)
      {
         visitor.Visit(this);
      }

      internal override StringBuilder ToString(StringBuilder builder, HashSet<ParseNode> seenNodes)
      {
         builder.Append(Name);
         return base.ToString(builder, seenNodes);
      }
   }

   public class Terminal
   {
      protected String mExpression;
      protected Boolean mEscape;

      protected Regex mRegex;
      protected SimpleRegex mSimpleRegex;

      protected ParseContext mCacheContext;
      protected String[] mMatchCache;

      public SimpleRegex SimpleRegex { get { return mSimpleRegex; } }

      // todo: remove once DefaultTerminal is no longer a GeneralTerminal.
      public Terminal() { }

      //public GeneralTerminal(String expr) : this(expr, false) { }

      //public GeneralTerminal(String expr, Boolean escape)
      //{
      //   mExpression = expr;
      //   mEscape = escape;

      //   // todo: perhaps just using Regex.Escape would be better
      //   mSimpleRegex = SimpleRegex.Parse(escape ? SimpleRegex.Escape(expr) : expr);
      //   mRegex = new Regex(escape ? Regex.Escape(expr) : expr, RegexOptions.CultureInvariant | RegexOptions.IgnorePatternWhitespace); // todo: options
      //}

      public Terminal(String expr, SimpleRegexBuilder regexBuilder)
      {
         mExpression = expr;
         mSimpleRegex = regexBuilder.Parse(expr);
         mRegex = new Regex(expr, RegexOptions.CultureInvariant | RegexOptions.IgnorePatternWhitespace); // todo: same as above
      }

      public virtual Terminal Clone()
      {
         Terminal tResult = new Terminal();
         tResult.mExpression = this.mExpression;
         tResult.mSimpleRegex = this.mSimpleRegex.Clone();
         tResult.mRegex = this.mRegex;
         return tResult;
      //   return new GeneralTerminal(mExpression, mEscape); // do diff?
      }

      protected Boolean CheckCache(ParseContext ctx, out String rCacheResult)
      {
         if (mMatchCache == null || ctx != mCacheContext)
         {
            mCacheContext = ctx;
            mMatchCache = new String[ctx.Expression.Length];
         }
         if (ctx.Position >= mMatchCache.Length)
         {
            rCacheResult = null;
            return false;
         }
         rCacheResult = mMatchCache[ctx.Position];
         return rCacheResult != null;
      }

      protected void SetCache(ParseContext ctx, Match pMatch)
      {
         mMatchCache[ctx.Position] = pMatch.Value;
      }

      protected Boolean CheckCache(ParseContext ctx)
      {
         String tDummy;
         return CheckCache(ctx, out tDummy);
      }

      public virtual Boolean CanAdvance(ParseContext ctx)
      {
         if (CheckCache(ctx))
            return true;

         Match tMatch = mRegex.Match(ctx.Expression, ctx.Position);
         if (tMatch.Success && tMatch.Index == ctx.Position)
         {
            SetCache(ctx, tMatch);
            return true;
         }
         return false;
      }

      public virtual Boolean OptAdvance(ParseContext ctx)
      {
         ctx.AdvanceInterleaved();

         String tResult;
         if (CheckCache(ctx, out tResult))
         {
            ctx.Position += tResult.Length;
            ctx.AdvanceInterleaved(); // todo.. dont need to
            return true;
         }

         Match tFirstMatch = mRegex.Match(ctx.Expression, ctx.Position);
         if (!tFirstMatch.Success)
            return false;
         if (tFirstMatch.Index != ctx.Position)
            return false;

         SetCache(ctx, tFirstMatch);

         ctx.Position += tFirstMatch.Length;

         ctx.AdvanceInterleaved();

         return true;
      }

      public virtual String AdvanceTerminal(ParseContext ctx)
      {
         ctx.AdvanceInterleaved();

         String tResult;
         if (CheckCache(ctx, out tResult))
         {
            ctx.Position += tResult.Length;
            ctx.AdvanceInterleaved(); // todo: don't need to do this, can cache better
            return tResult;
         }

         Match tFirstMatch = mRegex.Match(ctx.Expression, ctx.Position);
         if (!tFirstMatch.Success) // todo: check index as well?
            throw new ParseException("Failed to advanced terminel. Expected to match {0} starting at {1}: {2}", mRegex, ctx.Position.ToString(), ctx.Expression.Substring(ctx.Position)); // todo: improve

         SetCache(ctx, tFirstMatch);
         
         ctx.Position += tFirstMatch.Length;

         ctx.AdvanceInterleaved();
         
         return tFirstMatch.Value;
      }

      // todo: this is only used in or, do there explicitly?
      public virtual Boolean ConflictsWith(Terminal pOther)
      {
         return mSimpleRegex.SharesCommonPrefixWith(pOther.mSimpleRegex); 
      }

      public override string ToString()
      {
         return mExpression;
      }

      //// We need this for Terminal.ParsesTerminal etc.
      public Boolean SemanticEquals(object obj)
      {
         Terminal tOther = obj as Terminal;
         if (tOther == null)
            return false;

         if (this == tOther || tOther.mExpression == mExpression) // todo: case for insensitivity
            return true;

         // todo: we must clone here but should look at if we could avoid that somehow
         //if (SimpleRegex.RewritesEqual(this.mSimpleRegex, tOther.mSimpleRegex))
         if (this.mSimpleRegex.SemanticEquals(tOther.mSimpleRegex))
            return true;

         return false;
      }

      public override Boolean Equals(object obj)
      {
         return SemanticEquals(obj); // todo: if this ok, can remove of course
      }

      // Hashcode is expensive, but can be cached.
      protected Int32? mHashCode = null;

      public override int GetHashCode()
      {
         if (mHashCode.HasValue)
            return mHashCode.Value;

         // todo: this performs an unnecessary regex rewrite (!)
      //   mHashCode = this.mSimpleRegex.EqualsConsistentHashCode(); // todo: may wish to try 
         mHashCode = this.mSimpleRegex.EqualsConsistentHashCode();
         return mHashCode.Value;
      }

      // The following avoids having to store a dictionary lookup elsewhere.
      internal ParseNode ParentNode;
      protected ParseNode mParseNode;
      internal ParseNode Node 
      {
         get { return mParseNode; }
         set
         {
            if (mParseNode != null)
               throw new ParseException("Attempt to reuse terminal parse node.");

            mParseNode = value;
         }
      }
   }

   // todo: this is a proof of concept, must use an efficient algorithm instead.

   /* Datastructure representing the parsetable. It should do:
   * Given the input stream, start a search for characters as they are presented.
   * Once no more match is found, backtrack to longest result.
   * */

   // todo: idea
   // - currently a decision is made continuously, causing a a lot of unnecessary retrying..
   // - im not user if we could get that much faster, we still have to make the choice at each point, may be able to cache a prefix
   // (2):
   // - the or parsenode performs a dict lookup in its parse method, instead, the general terminal could store this target
   //   allowing for a direct "jump" to the correct spot
   public class ParseTable<T> where T : class
   {
      protected Dictionary<Terminal, T> Table = new Dictionary<Terminal, T>();

      //public void AddTerminal(String terminalStr, T node)
      //{
      //   AddTerminal(new GeneralTerminal(terminalStr), node);
      //}

      public void AddTerminal(Terminal pTerm, T node) 
      {
         if (pTerm == null)
            throw new ArgumentException("key empty or null");

         if (node == null)
            throw new ArgumentException("value is null");

         Table.Add(pTerm, node);
      }

      public Boolean ContainsTerminal(Terminal pTerminal)
      {
         return Table.ContainsKey(pTerminal);
      }

      // todo: do this properly
      public T GetMatchingTerminal(Terminal pTerminal)
      {
         //return Table.Keys.Any(k => k == pTerminal ||  ((GeneralTerminal)k).SemanticEquals(pTerminal));
         var tResult = from k in Table.Keys where k == pTerminal || k.SemanticEquals(pTerminal) select this[k];
         if (tResult.Count() > 1)
            throw new Exception("todo");
         return tResult.FirstOrDefault();
      }

      public Boolean HasConflictingTerminal(Terminal pTerminal)
      {
         //Boolean tResult = Table.Any(t => pTerminal.ConflictsWith(t.Key));
         //return tResult;

         foreach (Terminal tTerminal in Table.Keys)
            if (tTerminal != pTerminal && tTerminal.ConflictsWith(pTerminal))
               return true;

         return false;
      }

      public T this[Terminal pTerm]
      {
         get { return Table[pTerm]; }
         set 
         {
            if (value == null)
               throw new ArgumentNullException();

            Table[pTerm] = value; 
         }
      }

      public Boolean TryGetNode(Terminal pTerm, out T node)
      {
         return Table.TryGetValue(pTerm, out node);
      }

      public IEnumerable<Terminal> Terminals { get { return Table.Keys; } }
      public IEnumerable<KeyValuePair<Terminal, T>> KeyValues { get { return Table; } }
   }

   public abstract class ParserBase
   {
      public enum ParserState
      {
         None,
         InitializingRegexBuilder,
         Priming,
         Primed,
         Verified
      }

      protected ParserState State { get; set; }

      protected List<ProductionNode> _mPrimeTargets = new List<ProductionNode>();
      protected SimpleRegexBuilder mRegexBuilder;

      //protected HashSet<Char> mAlphabet = new HashSet<Char>();
      //protected HashSet<RangeRegex> mRangeSet = new HashSet<RangeRegex>();

      protected ParseNode Root;

      protected ParserBase()
      {
         State = ParserState.None;
         
         DefineGrammar();

         
      }

      protected ParseNode Define(Func<ParseNode> definition)
      {
         //ProductionNode tNode = new ProductionNode(definition);
         //_mPrimeTargets.Add(tNode);
         //return tNode;
         return Define((String)null, definition);
      }

      protected ParseNode Define(String terminal)
      {
         return Define(() => terminal.Terminal());
      }

      protected virtual ParseNode Define(String name, Func<ParseNode> definition)
      {
         ProductionNode tNode = new ProductionNode(name, definition);
         _mPrimeTargets.Add(tNode);
         return tNode;
      }

      protected ParseNode Define(String name, String terminal)
      {
         return Define(name, () => terminal.Terminal());
      }

      protected ParseNode Define(out ParseNode target, Func<ParseNode> definition)
      {
         return Define(out target, null, definition);
      }

      protected ParseNode Define(out ParseNode target, String terminal)
      {
         return Define(out target, () => terminal.Terminal());
      }

      protected ParseNode Define(out ParseNode target, String name, Func<ParseNode> definition)
      {
         return (target = Define(name, definition));
         //ProductionNode tNode = new ProductionNode(name, definition);
         //_mPrimeTargets.Add(tNode);
         //target = tNode;
         //return tNode;
      }

      protected ParseNode Define(out ParseNode target, String name, String terminal)
      {
         return Define(out target, name, () => terminal.Terminal());
      }

      /// <summary>
      /// Usage:
      /// 
      /// // Declare the node.
      /// ParseNode A;
      /// // Define it.
      /// Define(() => A, () => ... definition ... );
      /// 
      /// This is equivalent* to doing:
      /// Define(out A, "A", () => ... definition ... );
      /// 
      /// * note: this relies partly on the naming of the compiler generated field
      /// </summary>
      /// <param name="target"></param>
      /// <param name="definition"></param>
      /// <returns></returns>
      protected ParseNode Define(Expression<Func<ParseNode>> target, Func<ParseNode> definition)
      {
         MemberExpression tMemExpr = target.Body as MemberExpression;
         if (tMemExpr == null)
            throw new Exception("expected a member expression");

         ParseNode tResult = Define(tMemExpr.Member.Name, definition);

         // Perform assignment.
         ((Func<ParseNode>)Expression.Lambda(Expression.Assign(tMemExpr, Expression.Constant(tResult))).Compile())();
         return tResult;
      }

      protected ParseNode Define(Expression<Func<ParseNode>> target, Func<String> definition)
      {
         return Define(target, () => definition().Terminal());
      }

      protected ParseNode Define(Expression<Func<ParseNode>> target, String terminal)
      {
         return Define(target, () => terminal.Terminal());
      }

      protected abstract void DefineGrammar();

      public SimpleRegexBuilder RegexBuilder { get { return mRegexBuilder; } }

      protected abstract ParseContext GetContext();

      public ParserBase Build()
      {
         if (Root == null)
            throw new ParseException("Invalid grammar definition: Root was not assigned.");

         Root.SetName("Root"); // overrides anything set elsewhere

         if (State != ParserState.None)
            throw new ParseException("can only call Build() once");

         // Prime.
         State = ParserState.InitializingRegexBuilder;

         // Initial walk through to collect our alphabet and ranges to be able to init our regex builder.
         SimpleRegexBuilder tBuilder = new SimpleRegexBuilder();

         Root.InitRegexBuilder(tBuilder, this);

         mRegexBuilder = tBuilder.Build();

         // Second walk to "prime" our parser.
         State = ParserState.Priming;

         if (!Root.Prime(this))
            throw new ParseException("Failed to prime root. This could indicate your grammar is circular.");

         State = ParserState.Primed;

         // Second walk.
         // todo: also use this to verify all nodes are primed!
         Root.VerifyTree();

         // Verify all nodes were primed, if one was not, it's not reachable from the Root.
         // todo: note this won't catch things like "a".Terminal as it won't pass through Define, maybe find a good way to tackle that?
         foreach (ProductionNode tNode in _mPrimeTargets)
            if (!tNode.IsPrimed())
               throw new ParseException("unreachable terminal detected");

         State = ParserState.Verified;

         return this;
      }

      public SyntaxNode Parse(String expression)
      {
         ParseContext tContext = GetContext();
         tContext.Expression = expression; // todo: should pass to ctor i think

         return Root.Parse(tContext);
      }
   }

   // todo:
   // define a parser that uses a parsenode based approach to interleaving...
   //public class InterleavingParserBase : ParserBase
   //{
   //   protected ParseNode Interleaved;


   //}
}
