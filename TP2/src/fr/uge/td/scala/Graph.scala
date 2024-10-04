package fr.uge.td.scala

import scala.annotation.tailrec;

object Graph{
  val graph = List((1,0,5),(5,1,8),(8,2,1),(2,0,6),(3,0,6),(6,1,9),(5,1,9),(9,3,11),(9,4,12),(4,0,7),(7,1,9),(7,2,10),
    (14,1,15),(15,1,16),(14,1,16),(17,0,18),(18,0,19),(19,1,20),(20,0,17))

  val half = List((1,2),(3,4))
  val half2 = List((2,5),(1,2),(2,8),(5,7))


  def removeArc(l :List[(Int,Int,Int)]) : List[(Int,Int)]={
    l.map{case(x,_,z)=>(x,z)}
  }

  def rootNode(l: List[(Int,Int)]):List[Int] = {
    val list = l.map{case(x,_)=>x}
    val list2 = l.map{case(_,x)=>x}
    list.filter(x=> !list2.contains(x)).distinct
  }

  def leafNode(l:List[(Int,Int)]):List[Int]={
    val list = l.map{case(x,_)=>x}
    val list2 = l.map{case(_,x)=>x}
    list2.filter(x=> !list.contains(x)).distinct
  }

  def join(pair1 : List[(Int,Int)], pair2: List[(Int,Int)]) : List[(Int,Int)] = {
    var pair = pair1
    var tmp = List[(Int,Int)]()
    do{
      tmp = pair;
      pair = (pair.flatMap { case (x, y) => pair2.map { case (a, b) => if (a == y) (x, b) else (0, 0)}.filterNot(_ == (0, 0)) }:++pair2:++pair1).distinct
    }while(pair.length != tmp.length);
    pair
  }

  def inferpair( pair1 : List[(Int,Int)], pair2: List[(Int,Int)]): List[(Int,Int)]={
    join(pair1,pair2).filter(x=> !pair1.contains(x) && !pair2.contains(x))
  }

  def rootedGraph(root : Int):List[(Int,Int)]={
    val removedArc = removeArc(graph);
    if(!rootNode(removedArc).contains(root)) List[(Int,Int)]()
    val pair1 = removedArc.filter{case(x,_)=> x==root };
    join(pair1,removedArc).filter(x=> x._1 ==root)
  }


  def joinv2(pair1 : List[(Int,Int)], pair2 : List[(Int,Int)]): List[(Int,Int)] ={
      @tailrec
      def joinRecursive(current: List[(Int,Int)], previous : List[(Int,Int)]) : List[(Int,Int)]={
        if(current.length==previous.length) current
        else joinRecursive((current.flatMap{case (x, y) => pair2.map { case (a, b) => if (a == y) (x, b) else (0, 0)}.filterNot(_ == (0, 0))}:++pair2:++pair1).distinct,current)
      }
    joinRecursive(pair1,List.empty)
  }

  def transitiveClosure(graph : List[(Int,Int,Int)]):List[(Int,Int)]={
    val pairs = removeArc(graph)
    inferpair(pairs,pairs)
    //joinv2(pairs,pairs)
  }

  def main(args: Array[String]): Unit = {
    val removed =removeArc(graph)
    println(s"${removed}")
    val root = rootNode(removed)
    println(s"${root}")
    val leaf = leafNode(removed)
    println(s"${leaf}")
    val joined = join(half,half2)
    println(s"${joined}")
    val inferJoined = inferpair(half,half2)
    println(s"${inferJoined}")
    println("-----------")
    val rootGraph = rootedGraph(1)
    println(s"${rootGraph}")
    val joined2 = joinv2(half,half2)
    println(s"${joined2}")
    val transitive = transitiveClosure(graph)
    println(s"${transitive}")
  }
}