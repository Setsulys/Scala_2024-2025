package fr.uge.td.scala
import java.io.RandomAccessFile
class HashTable(val fileBdd :String,val offset : Int,val threshold: Double){
  var hashMap = Map[String,Int]()


  def addToMap()={
    val fileReader = new RandomAccessFile(fileBdd,"rw");
    val readed = fileReader.readUTF().split("",offset)
    var current:Int=0
    readed.foreach(e=> {
      hashMap = hashMap + (e.split(":")(0) -> current)
      current+=offset
    })
    fileReader.close()
  }

  def overrideFile(): Unit = {
    val newFile = "new"+fileBdd
    val fileWriter = new RandomAccessFile(newFile,"rw")
    addToMap()
    hashMap.foreach(k => {
      val keyValue: String = k._1 + ":"+get(k._1).toString
      val nbStar = offset -keyValue.length
      keyValue:++"*"*nbStar
      fileWriter.writeUTF(keyValue)
    })
    fileWriter.close()
  }

  def get(key : String) : Option[Int] ={
    val value = hashMap.get(key)
    val find = new RandomAccessFile(fileBdd,"rw")
    find.seek(value.get)
    val buffer = new Array[Byte](offset)
    find.read(buffer)
    find.close()
    Option(new String(buffer).replace("*","").toInt)
  }

  def checkThresHold(): Unit = {
    val fileReader = new RandomAccessFile(fileBdd,"rw");
    if(fileReader.length()>0){
      val readed = fileReader.readUTF().split("",offset)
      if(hashMap.nonEmpty){
        if(readed.length / hashMap.size >threshold){
          overrideFile()
        }
      }
    }

  }

  def add(key:String, value:Int): Unit={
    checkThresHold()
      val keyValue : String = key+":"+ value
      val nbStar = offset - keyValue.length
      keyValue:++"*"*nbStar
      val file = new RandomAccessFile(fileBdd,"rw")
      file.writeUTF(keyValue)
  }



}



object HashTable{
  def main(args: Array[String]): Unit = {
    val hash: HashTable = new HashTable("txt.txt",20,0.4)
    hash.add("abc",777)
    hash.add("bcd",546)
  }
}