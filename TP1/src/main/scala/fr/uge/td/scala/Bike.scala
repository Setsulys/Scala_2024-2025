package fr.uge.td.scala

class Bike (var color:String,var wheelSize:Int,var speed:Int=0){

  def speedUp(value:Int): Unit = {
    if(value < 0) throw new IllegalArgumentException("negative speed");
    speed+=value
  };
  def break(value:Int): Unit = {
    if(value < 0) throw new IllegalArgumentException("negative speed");
    speed-=value
  };


}

object Bike{
  def main(args:Array[String]):Unit={
    val bike = new Bike("red",20);
    println(s"Bike color is  -> ${bike.color}");
    bike.speedUp(10);
    println(s"Bike speed is -> ${bike.speed}")
    bike.break(2);
//    println(s"Bike speed is -> ${bike.speed}")
//    bike.break(-2);

  }
}