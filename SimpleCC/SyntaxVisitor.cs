using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace SimpleCC
{
   public abstract class SyntaxVisitor
   {
      public virtual void Visit(ChoiceSyntaxNode pNode) { }
      public virtual void Visit(SequenceSyntaxNode pNode) { }
      public virtual void Visit(ValueSyntaxNode pNode) { }
      public virtual void Visit(EofSyntaxNode pNode) { }
      public virtual void Visit(ProductionSyntaxNode pNode) { }
      public virtual void Visit(OptionalSyntaxNode pNode) { }
   }

   public abstract class SyntaxVisitor<V>
   {
      public virtual V Visit(ChoiceSyntaxNode pNode) { return default(V); }
      public virtual V Visit(SequenceSyntaxNode pNode) { return default(V); }
      public virtual V Visit(ValueSyntaxNode pNode) { return default(V); }
      public virtual V Visit(EofSyntaxNode pNode) { return default(V); }
      public virtual V Visit(ProductionSyntaxNode pNode) { return default(V); }
      public virtual V Visit(OptionalSyntaxNode pNode) { return default(V); }
   }
}
