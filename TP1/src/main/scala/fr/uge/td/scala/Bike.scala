package fr.uge.td.scala

class Bike (val color:String,val wheelSize:Int,val speed:Int=0){

  def speedUp(value:Int): Either[String,Bike] = {
    if(value < 0) Left("negative value")
    else Right(new Bike(color, wheelSize, speed+value))
  };
  def break(value:Int): Either[String,Bike] = {
    if(value < 0) Left("negative value")
    else Right(new Bike(color, wheelSize, speed-value))
  };
}

object Bike{


  def main(args:Array[String]):Unit={
    val bike = new Bike("red",20);
    println(s"Bike color is  -> ${bike.color}")
    println(s"Bike speed is ${bike.speed}")
    val bike2 = bike.speedUp(10);
    bike2 match{
      case Left("negative value")=> println("Error");
      case Right(value)=> println(s"Bike speed is ${value.speed}")
        val bike3 = value.speedUp(-1);
      bike3 match {
        case Left("negative value")=> println("Error");
        case Right(value) => println(s"Bike speed is ${value.speed}");
          val bike4 = value.break(5);
      }
    }
    println("new bike")
    bike2 match{
      case Left("negative value")=> println("Error");
      case Right(value)=> println(s"Bike speed is ${value.speed}")
        val bike3 = value.speedUp(10);
        bike3 match {
          case Left("negative value")=> println("Error");
          case Right(value) => println(s"Bike speed is ${value.speed}");
            val bike4 = value.break(5);
            bike4 match {
              case Left("negative value")=> println("Error");
              case Right(value) => println(s"Bike speed is ${value.speed}");
            }
        }
    }
  }
}