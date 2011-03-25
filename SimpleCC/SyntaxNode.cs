using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Linq.Expressions;

namespace SimpleCC
{
   // Result graph.
   // I have chosen not to reuse ParseNode for these as they serve quite a different purpose.
   // Also, this makes rewriting easier as it should be straightforward to add such nodes etc.
   public abstract class SyntaxNode
   {
      // Name of node, possibly null.
      public String Name { get; set; }

      // Children.
      public SyntaxNode[] Children { get; set; }

      public Object Value;

      public abstract void Accept(SyntaxVisitor pVisitor);
      public abstract V Accept<V>(SyntaxVisitor<V> pVisitor);

      public Func<SyntaxNode, Expression> Compiler;

      public virtual Expression Compile()
      {
         if (Compiler == null)
            throw new InvalidOperationException("No compiler defined.");

         return Compiler(this);
      }

      public override string ToString()
      {
         SyntaxToStringVisitor tVisitor = new SyntaxToStringVisitor();
         this.Accept(tVisitor);
         return tVisitor.ToString();
      }
   }

   public class EofSyntaxNode : SyntaxNode
   {
      public override void Accept(SyntaxVisitor pVisitor)
      {
         pVisitor.Visit(this);
      }

      public override V Accept<V>(SyntaxVisitor<V> pVisitor)
      {
         return pVisitor.Visit(this);
      }
   }

   public class ProductionSyntaxNode : SyntaxNode
   {
      public ParseNode Production { get; set; }

      public override void Accept(SyntaxVisitor pVisitor)
      {
         pVisitor.Visit(this);
      }

      public override V Accept<V>(SyntaxVisitor<V> pVisitor)
      {
         return pVisitor.Visit(this);
      }
   }

   public class ChoiceSyntaxNode : SyntaxNode
   {
      public override void Accept(SyntaxVisitor pVisitor)
      {
         pVisitor.Visit(this);
      }

      public override V Accept<V>(SyntaxVisitor<V> pVisitor)
      {
         return pVisitor.Visit(this);
      }
   }

   public class SequenceSyntaxNode : SyntaxNode
   {
      public override void Accept(SyntaxVisitor pVisitor)
      {
         pVisitor.Visit(this);
      }

      public override V Accept<V>(SyntaxVisitor<V> pVisitor)
      {
         return pVisitor.Visit(this);
      }
   }

   public class OptionalSyntaxNode : SyntaxNode
   {
      public override void Accept(SyntaxVisitor pVisitor)
      {
         pVisitor.Visit(this);
      }

      public override V Accept<V>(SyntaxVisitor<V> pVisitor)
      {
         return pVisitor.Visit(this);
      }
   }

   public class ValueSyntaxNode : SyntaxNode
   {
      public override void Accept(SyntaxVisitor pVisitor)
      {
         pVisitor.Visit(this);
      }

      public override V Accept<V>(SyntaxVisitor<V> pVisitor)
      {
         return pVisitor.Visit(this);
      }
   }
}
