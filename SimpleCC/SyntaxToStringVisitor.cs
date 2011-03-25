using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace SimpleCC
{
   public class SyntaxToStringVisitor : SyntaxVisitor
   {
      protected StringBuilder mBuffer = new StringBuilder();

      public override void Visit(ChoiceSyntaxNode pNode)
      {
         pNode.Children[0].Accept(this);
      }

      public override void Visit(OptionalSyntaxNode pNode)
      {
         if (pNode.Children != null && pNode.Children.Length > 0)
         {
            mBuffer.Append("OPTIONAL(");
            Boolean tSuffix = false;
            foreach (SyntaxNode tChild in pNode.Children)
            {
               if (tSuffix)
                  mBuffer.Append(", ");

               Int32 tPrevLen = mBuffer.Length;

               tChild.Accept(this);

               tSuffix = tPrevLen != mBuffer.Length;
            }
            mBuffer.Append(")");
         }
      }

      public override void Visit(ValueSyntaxNode pNode)
      {
         mBuffer.Append("'").Append(pNode.Value.ToString()).Append("'");
      }

      public override void Visit(EofSyntaxNode pNode)
      {
         mBuffer.Append("<<EOF>>");
      }

      public override void Visit(ProductionSyntaxNode pNode)
      {
         mBuffer.Append("(" + (pNode.Name ?? "") + ")").Append("-> ");
         if (pNode.Children != null && pNode.Children.Length > 0)
         {
            Boolean tSuffix = false;
            foreach (SyntaxNode tChild in pNode.Children.Where(n => n != null))
            {
               if (tSuffix)
                  mBuffer.Append(", ");

               tChild.Accept(this);
               tSuffix = true;
            }
         }
         else
            mBuffer.Append(pNode.Value ?? "''");
      }

      public override void Visit(SequenceSyntaxNode pNode)
      {
         mBuffer.Append("FOLLOW(");
         foreach (SyntaxNode tChild in pNode.Children)
         {
            tChild.Accept(this);
            mBuffer.Append(", ");
         }
         mBuffer.Append(")");
      }

      public override string ToString()
      {
         return mBuffer.ToString();
      }
   }
}
