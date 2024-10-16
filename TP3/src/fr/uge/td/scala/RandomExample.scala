package fr.uge.td.scala

import java.util.TimerTask
import scala.util.Random

class RandomExample (val hashTable: HashTable) extends TimerTask{
  val keys = List("abc","def","gag","gae","rrh","gea","gge","ghe")
  override  def run(): Unit = {
    val randomKeu = Random.nextInt(keys.length)
    val randomValues = Random.nextInt(999999999)
    hashTable.add(keys(randomKeu),randomValues)
  }

}
