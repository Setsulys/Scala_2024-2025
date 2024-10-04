package fr.uge.td.scala
import java.io.RandomAccessFile
class HashTable(val fileBdd :String,val offset : Int,val threshold: Double){
  var hashMap = Map[String,Int]()
  private val fileReader = new RandomAccessFile(fileBdd,"rw")
  var addIndex= 0


  def closeFile()={
    fileReader.close()
  }

  def overrideFile()={
    var str :String = ""
    var buffer = new Array[Byte](offset)
    addIndex=0
    hashMap.foreach(e=> {
      fileReader.seek(e._2)
      fileReader.read(buffer)
      str = str++:new String(buffer)
      hashMap = hashMap + (e._1-> addIndex)
      addIndex+= offset
    })
    val resBuffer = str.getBytes
    fileReader.setLength(0)
    fileReader.write(resBuffer,0,resBuffer.length)
  }

  def get(key : String) : Option[Int] ={
    val value = hashMap.get(key)
    if(value.isDefined){
      val buffer = new Array[Byte](offset)
      fileReader.seek(value.get)
      fileReader.read(buffer)
      Some(new String(buffer).replaceAll("\\*", "").split(":")(1).toInt)
    }
    else None
  }

  def checkThresHold(): Unit = {
    if(hashMap.nonEmpty) {
      println(s"${(hashMap.size.toDouble*offset / addIndex.toDouble) }")
      if((hashMap.size.toDouble*offset / addIndex.toDouble) < threshold){
        println("hey")
        overrideFile()
      }
    }

  }

  def add(key:String, value:Int): Unit={
    checkThresHold()
    val keyValue : String = key+":"+ value
    val nbStar = offset - keyValue.length
    val res = keyValue:++"*"*nbStar
    hashMap = hashMap + (key-> addIndex)
    addIndex+=offset
    fileReader.seek(fileReader.length)
    fileReader.write(res.getBytes())

  }



}



object HashTable{
  def main(args: Array[String]): Unit = {
    val hash: HashTable = new HashTable("txt.txt",20,0.4)
    hash.add("abc",777)
    hash.add("def",541)
    hash.add("def",542)
    hash.add("def",543)
    hash.add("def",544)
    hash.add("def",545)
    hash.add("def",546)
    hash.add("abc",771)
    hash.add("abc",772)
    hash.add("abc",773)
    hash.add("abc",774)


    val res = hash.get("def")
    println(s"${res}")
    println(s"${hash.hashMap}")
    hash.closeFile()
  }
}