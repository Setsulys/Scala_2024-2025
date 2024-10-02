package fr.uge.td.scala;

object Graph{
  val graph = List((1,0,5),(5,1,8),(8,2,1),(2,0,6),(3,0,6),(6,1,9),(5,1,9),(9,3,11),(9,4,12),(4,0,7),(7,1,9),(7,2,10),
    (14,1,15),(15,1,16),(14,1,16),(17,0,18),(18,0,19),(19,1,20),(20,0,17))

  val half = List((1,2),(3,4))
  val half2 = List((2,5),(1,2),(2,8))

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
    val pair = List[(Int,Int)]()
    pair1.map{case(x,y)=> pair2.map{case(a,b)=> if (b==x) (a,y) else (0,0)}}
  }

  def main(args: Array[String]): Unit = {
    val removed =removeArc(graph)
    println(s"${removed}")
    val root = rootNode(removed)
    println(s"${root}")
    val leaf = leafNode(removed)
    println(s"${leaf}")
    /*val joined = join(half,half2)
    println(s"${joined}")*/

  }
}