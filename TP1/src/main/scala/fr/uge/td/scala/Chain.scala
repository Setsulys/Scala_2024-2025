package fr.uge.td.scala


sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

def tail[A](list:List[A]):List[A]={
  list match{
    case Nil => Nil
    case Cons(head,tail)=> tail
  }
}

object Chain {
  def main(args: Array[String]): Unit = {
    val chained = Cons(1,Cons(2,Cons(3,Nil)));
    println(s"${tail(chained)}")
  }
}

/**
 * Exercice 2.1
 * Sealed : Sealed permet d'eviter de faire de l'heritage, sauf quand on est dans la meme classe
 * case class: En utilisant case class on n'a pas besoin de mettre de new a la creation, et cree un constructeur a la création
 * tout les paramettre seront en val donc immuable et la comparaison est par rapport a la struct et non la ref
 * Nothing: Nothing est le type le plus bas de scala, c'est un sous type de tout les type de scala a l'inverse de Any
 *
 */