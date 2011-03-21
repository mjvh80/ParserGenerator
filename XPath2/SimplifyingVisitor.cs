using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Parser;

namespace Parser
{
   public class FlatteningVisitor : SyntaxVisitor
   {
      protected List<SyntaxNode> mChildren = new List<SyntaxNode>();

      public SyntaxNode Flatten(ProductionSyntaxNode pNode)
      {
         Flatten((SyntaxNode)pNode); // apply to kids
         pNode.Children = mChildren.ToArray();
         return pNode;
      }

      //protected void Flatten(SyntaxNode pNode)
      //{
      //   if (pNode == null || pNode.Children == null || pNode.Children.Length == 0)
      //      return;

      //   foreach (SyntaxNode tChild in pNode.Children)
      //      tChild.Accept(this);
      //}

      protected void Flatten(SyntaxNode pNode)
      {
         if (pNode == null || pNode.Children == null || pNode.Children.Length == 0)
            return;

         foreach (SyntaxNode tChild in pNode.Children)
            if (tChild != null)
               tChild.Accept(this);
      }

      public override void Visit(ChoiceSyntaxNode pNode)
      {
         Flatten(pNode);
      }

      public override void Visit(EofSyntaxNode pNode) { }

      public override void Visit(OptionalSyntaxNode pNode)
      {
         Flatten(pNode);
      }

      public override void Visit(ValueSyntaxNode pNode)
      {
         if (pNode.Value != null)
            mChildren.Add(pNode);
      }

      public override void Visit(SequenceSyntaxNode pNode)
      {
         Flatten(pNode);
      }

      public override void Visit(ProductionSyntaxNode pNode)
      {
         FlatteningVisitor tVisitor = new FlatteningVisitor();
         tVisitor.Flatten(pNode);
         pNode.Children = tVisitor.mChildren.ToArray();
         mChildren.Add(pNode);

         /*
         List<SyntaxNode> tExpandedKids = new List<SyntaxNode>();
         ;
         foreach (SyntaxNode tChild in pNode.Children)
         {
            SyntaxNode tFlattenedChild = tChild.Accept(this);
            if (tFlattenedChild != null && tFlattenedChild.Children != null)
               tExpandedKids.AddRange(tFlattenedChild.Children);
         }

         pNode.Children = tExpandedKids.ToArray();
         */
      }
   }

   public class SimplifyingVisitor : SyntaxVisitor<SyntaxNode>
   {
      public SyntaxNode Simplify(SyntaxNode pNode)
      {
         return pNode.Accept(this);
      }

      public override SyntaxNode Visit(ChoiceSyntaxNode pNode)
      {
         if (pNode.Children[0] == null)
            return null; // special value indicating no interest (eg empty option)
            
         // Return simplified child.
         return pNode.Children[0].Accept(this);
      }

      public override SyntaxNode Visit(EofSyntaxNode pNode)
      {
         // Don't really care about EOF.
         return null;
      }

      public override SyntaxNode Visit(OptionalSyntaxNode pNode)
      {
         if (pNode.Children == null || pNode.Children.Length == 0)
            return null; // not very interesting is it

         // Process kids.
         for (Int32 i = 0; i < pNode.Children.Length; i++)
            pNode.Children[i] = pNode.Children[i].Accept(this);

         // Filter null.
         pNode.Children = pNode.Children.Where(n => n != null).ToArray(); // (from c in pNode.Children where c != null select c).ToArray();

         if (pNode.Children.Length == 0)
            return null;
         else if (pNode.Children.Length == 1)
            return pNode.Children[0];
         else
            return pNode;
      }

      public override SyntaxNode Visit(ProductionSyntaxNode pNode)
      {
         if (pNode.Children == null || pNode.Children.Length == 0 || pNode.Children[0] == null)
            return null;

         pNode.Children[0] = pNode.Children[0].Accept(this);

         if (pNode.Children[0] == null)
            return null;

         // Copy value, remove kids.
         if (pNode.Children.Length == 1 && pNode.Children[0] is ValueSyntaxNode)
         {
            pNode.Value = pNode.Children[0].Value;
            pNode.Children = null;
         }

         return pNode;
      }

      public override SyntaxNode Visit(SequenceSyntaxNode pNode)
      {
         if (pNode.Children == null || pNode.Children.Length == 0)
            return null; // not very interesting is it

         // Process kids.
         for (Int32 i = 0; i < pNode.Children.Length; i++)
            pNode.Children[i] = pNode.Children[i].Accept(this);

         // Unwrap sequences, todo do this more efficient.
         /*
         if (pNode.Children.Any(n => n is SequenceSyntaxNode))
         {
            Int32 tElementsNeeded = pNode.Children.Where(n => n is SequenceSyntaxNode).Select(n => n.Children.Length).Aggregate((t, i) => t + i);
        
            SyntaxNode[] tNewKids = new SyntaxNode[pNode.Children.Length + tElementsNeeded];
            for (Int32 i = 0, k = 0; i < pNode.Children.Length; i++)
            {
               if (pNode.Children[i] is SequenceSyntaxNode)
                  for (Int32 j = 0; j < pNode.Children[i].Children.Length; j++)
                     tNewKids[k++] = pNode.Children[i].Children[j];
               else
                  tNewKids[k++] = pNode.Children[i];
            }

            pNode.Children = tNewKids;
         }
          * */

         // Filter null.
         pNode.Children = pNode.Children.Where(n => n != null).ToArray(); // (from c in pNode.Children where c != null select c).ToArray();

         if (pNode.Children.Length == 0)
            return null;
         else if (pNode.Children.Length == 1)
            return pNode.Children[0];
         else
            return pNode;
      }

      public override SyntaxNode Visit(ValueSyntaxNode pNode)
      {
         return pNode.Value == null ? null : pNode;
      }
   }
}
