import ar.uba.dc.cap.Type
import ar.uba.dc.cap.Term
import ar.uba.dc.cap.Pattern
import ar.uba.dc.cap.TypeNode

package object cap {
  type Label = String
  type Index = Int
  type Ctx = Map[Label, Type]
  type Assignment = (Pattern, TypeNode)
  
  abstract class Sort
  case object D extends Sort
  case object T extends Sort
  
}