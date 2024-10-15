package fr.uge.td.scala
import java.io.RandomAccessFile
import java.nio.file.{Files, Paths, StandardCopyOption}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
class HashTable(val offset : Int,val threshold: Double){
  val currentFile = "current_log"
  var hashMap = Map[String,Int]()
  private val fileReader = new RandomAccessFile(currentFile,"rw")
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

    val fileTitle : String = "log_segment_"+DateTimeFormatter.ofPattern("yyyy-MM-dd_HH:mm:ss").format(LocalDateTime.now)
    Files.copy(Paths.get(currentFile),Paths.get(fileTitle),StandardCopyOption.REPLACE_EXISTING)
    fileReader.setLength(0)
    fileReader.write(resBuffer,0,resBuffer.length)
  }

  def get(key : String) : Option[Int] ={
    val value = hashMap.get(key)
    if(value.isDefined){
      val buffer = new Array[Byte](offset)
      fileReader.seek(value.get)
      fileReader.read(buffer)
      Some(new String(buffer).replaceAll("\\*", "").toInt)
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
    val keyValue : String= value.toString
    val nbStar = offset - keyValue.length
    val res = keyValue:++"*"*nbStar
    hashMap = hashMap + (key-> addIndex)
    addIndex+=offset
    fileReader.seek(fileReader.length)
    fileReader.write(res.getBytes())
    checkThresHold()
  }



}



object HashTable{
  def main(args: Array[String]): Unit = {
    val hash: HashTable = new HashTable(20,0.4)
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