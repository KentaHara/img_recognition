import org.opencv.core.{Core, Mat, CvType, Scalar, Size, Rect}
import org.opencv.highgui.Highgui
import org.opencv.imgproc.Imgproc
import java.io._

class Init {

  type OptionImgList      = Map[String, Option[Mat]]
  val InitOptionImgList   = Map[String, Option[Mat]]()
  type OptionScoreList    = Map[String, Option[Double]]
  val InitOptionScoreList = Map[String, Option[Double]]()

  var size         = new Size(320, 50)
  var bunner_size  = new Size(320, 50)
  val test_img_dir = "img"
  val mean_file_name = "mean.csv"
  val eigenvector_file_name = "eigenvector.csv"
  val model_uri = getClass.getResource("/model/").getPath 
  val init_file = "init.txt"

  def getResourceFileName(dir: String): List[String] = {
    val uri = getClass.getResource("/"+dir+"/").getPath
    val filelist = new File(uri).list
    var list = List[String]()
    filelist.foreach(s=>{list = (uri+s)::list})
    return list
  }
  
  def getResourceFileNameMap(dir: String): Map[String, String] = {
    val uri = getClass.getResource("/"+dir+"/").getPath
    val filelist = new File(uri).list
    var map = Map[String, String]()
    filelist.foreach(s=>{map += s->(uri+s)})
    return map
  }

  def setResize(value: String): Unit = {
    value split ',' match {
      case Array(row, col) => this.size = new Size(row.toInt, col.toInt)
      case _               => println("check define size : Ex resize_size=320,50")
    }
  }

  def setBunnerSize(value: String): Unit = {
    value split ',' match {
      case Array(row, col) => this.bunner_size = new Size(row.toInt, col.toInt)
      case _               => println("check define bunner_size : Ex bunner_size=320,50")
    }
  }

  def warn_println(value: String): Unit = println("[warning] " + value)

  def sets(value: String): Unit = {
    value split '=' match {
      case Array("resize_size", init) => setResize(init)
      case Array("bunner_size", init) => setBunnerSize(init)
      case Array(param, init)         => warn_println("can not use \"" + param + "\" parameter in init.txt")
      case _                          => None
    }
  }

  //constructor :get init file
  try{
    import scala.io.Source
    val init_source = Source.fromFile(getClass.getResource("/init/").getPath + init_file, "UTF-8")
    init_source.getLines.foreach(s => sets(s))
  } catch {
    case ex: Exception => println(ex)
  }
}

object TestImages extends Init {

  def getImages(): List[Mat] = {
    var matList = List[Mat]()
    getResourceFileName(test_img_dir).foreach(filePath => {
      val src = Highgui.imread(filePath, 0)
      if(src.cols > 0) {
        Imgproc.resize(src, src, size)
        matList = matList :+ MatTool.convertOneDimensionMatFromMat(src)
      }
    })
    return matList
  }
  
  def getOptionImages(): OptionImgList = {
    var mat_list = InitOptionImgList
    val filename_list = getResourceFileNameMap(test_img_dir)
    filename_list.keys.foreach(key => {
      val filePath = filename_list.get(key).get
     // println(filePath)
      val src = Highgui.imread(filePath, 0)
      if(src.cols > 0) {
        Imgproc.resize(src, src, size)
        mat_list += key -> Option(MatTool.convertOneDimensionMatFromMat(src))
      }
    })
    return mat_list
  }
}
