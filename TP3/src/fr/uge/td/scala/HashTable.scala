package fr.uge.td.scala
import java.io.{File, FilenameFilter, RandomAccessFile}
import java.nio.file.{Files, Paths, StandardCopyOption}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util
import java.util.Timer
import scala.collection.mutable
class HashTable(val offset : Int, val threshold :Int){
  private val currentFile = "active_log"
  private val archiveFile = "log-"
  val hashMap: mutable.Map[String, Int] = mutable.Map[String,Int]()
  private val fileReader = new RandomAccessFile(currentFile,"rw")
  private val keySize = 4
  private var addIndex= 0
  private var occurence = 1


  def closeFile():Unit={
    fileReader.close()
  }

/*  private def checkFileExist(fileTitle : String): String = {
    if(Files.exists(Paths.get(fileTitle))){
      val tempFileTitle = fileTitle+"("+occurence+")"
      occurence+=1
      tempFileTitle
    }
    else {
      occurence = 1
      fileTitle
    }
  }*/

  private def overrideFile():Unit={
    var str :String = ""
    val fileTitle = archiveFile+DateTimeFormatter.ofPattern("yyyyMMddHHmmssSSS").format(LocalDateTime.now)
    Files.copy(Paths.get(currentFile),Paths.get(fileTitle),StandardCopyOption.REPLACE_EXISTING)
    addIndex=0
    hashMap.clear()
    fileReader.setLength(0)
  }

  def get(key : String) : Option[Int] ={
    val value = hashMap.get(key)
    if(value.isDefined){
      val buffer = new Array[Byte](offset)
      fileReader.seek(value.get)
      fileReader.read(buffer)
      Some(new String(buffer).replaceAll("\\*", "").split(":")(1).toInt)
    }
    else{
      getOldFiles(key)
    }
  }

  def getOldFiles(key:String): Option[Int] = {
    val foundFiles = new File(".").listFiles(new FilenameFilter {
      override def accept(file: File, s: String): Boolean = s.startsWith(archiveFile)
    })
    val list = foundFiles.toList.sorted.reverse
    var toGet = new String()
    for(e <- list ){
      val file = new RandomAccessFile(e,"rw")
      for(i <- 0 until  threshold if i%offset==0){
        val buffer = new Array[Byte](offset)
        file.seek(i)
        file.read(buffer)
        val data = new String(buffer)
        if(data.startsWith(key)){
          toGet = data.split(":")(1).replace("*","")
        }
      }
      if(toGet.nonEmpty){
        return Some(toGet.toInt)
      }
    }
  None
  }

  private def checkThreshold(): Unit = {
    if(hashMap.nonEmpty) {
      if(fileReader.length()+offset > threshold){
        overrideFile()
      }
    }
  }

  def add(key:String, value:Int): Either[String,Unit]={
    if(key.length != 3) Left("Wrong key size")
    else if (value.toString.length > offset-keySize) Left("value too great")
    else{
      val keyValue : String= key+":"+value.toString
      val nbStar = offset - keyValue.length
      val res = keyValue:++"*"*nbStar
      hashMap(key) = addIndex
      addIndex+=offset
      fileReader.seek(fileReader.length)
      fileReader.write(res.getBytes())
      checkThreshold()
      Right()
    }
  }
}




object HashTable{
  def main(args: Array[String]): Unit = {
    val hash: HashTable = new HashTable(20,320)
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
    println(s"hello ${res}")
    val ref = hash.get("abc")
    println(s"hello ${ref}")
    //println(s"${hash.hashMap}")
    val timer = new Timer()
    timer.schedule(new RandomExample(hash),10,500)

    val rees = hash.get("gag")
    println(s"hello ${rees}")
    val reef = hash.get("rrh")
    println(s"hello ${reef}")
    hash.closeFile()
  }
}