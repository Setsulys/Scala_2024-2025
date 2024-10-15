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
  var occurence = 1


  def closeFile():Unit={
    fileReader.close()
  }

  def checkFileExist(fileTitle : String): String = {
    if(Files.exists(Paths.get(fileTitle))){
      var tempFileTitle = fileTitle
      tempFileTitle+="("+occurence+")"
      occurence+=1
      tempFileTitle
    }
    else {
      occurence = 1
      fileTitle

    }
  }

  private def overrideFile():Unit={
    var str :String = ""
    var fileTitle : String = "log_segment_"+DateTimeFormatter.ofPattern("yyyy-MM-dd_HH:mm:ss").format(LocalDateTime.now)
    fileTitle = checkFileExist(fileTitle)
    Files.copy(Paths.get(currentFile),Paths.get(fileTitle),StandardCopyOption.REPLACE_EXISTING)
    val buffer = new Array[Byte](offset)
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
      Some(new String(buffer).replaceAll("\\*", "").toInt)
    }
    else None
  }

  private def checkThresHold(): Unit = {
    if(hashMap.nonEmpty) {
      if((hashMap.size.toDouble*offset / addIndex.toDouble) < threshold){
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
    hash.add("abc",100)
    hash.add("def",1)
    hash.add("def",2)
    hash.add("def",3)
    hash.add("def",4)
    hash.add("def",5)
    hash.add("def",6)
    hash.add("abc",200)
    hash.add("abc",300)
    hash.add("abc",400)
    hash.add("abc",500)
    hash.add("def",9)
    hash.add("def",10)
    hash.add("def",11)
    hash.add("def",12)
    hash.add("def",13)
    hash.add("def",14)


    val res = hash.get("def")
    println(s"${res}")
    val ref = hash.get("abc")
    println(s"${ref}")
    println(s"${hash.hashMap}")
    hash.closeFile()
  }
}