package fr.uge.td.scala

sealed trait Tree[+A]
case object Nil1 extends Tree[Nothing]
case class Cons1[+A](value: A, left: Tree[A],right :Tree[A]) extends Tree[A]



object Tree {

  def size[A](t : Tree[A]): Int ={
    t match{
      case Nil1 => 0
      case Cons1(value,left,right)=> 1+size(left)+size(right);
    }
  }


  def main(args: Array[String]): Unit = {
    val treeLRL =Cons1(3,Nil1,Nil1);
    val treeLL = Cons1(2,Nil1,Nil1);
    val treeLR = Cons1(2,treeLRL,Nil1);
    val treeL = Cons1(1,treeLL,treeLR);
    val treeR = Cons1(2,Nil1,Nil1);
    val tree = Cons1(0,treeL,treeR);
    println(s"hauteur de l'arbre ${size(tree)}")
  }

}