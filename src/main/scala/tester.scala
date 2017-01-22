import org.opencv.core.{Core, Mat, CvType, Scalar, Size, Rect}
import org.opencv.highgui.Highgui
import org.opencv.imgproc.Imgproc
import java.io._

object TrainingModel extends Init {
  def main(args:Array[String]) {
    System.loadLibrary(Core.NATIVE_LIBRARY_NAME)
    
    val matList = TestImages.getOptionImages()

    val imgMat = MatTool.convertMapFromOneDimensionMats(matList)
    var mean = new Mat()
    var eigenvector = new Mat()
    val result = Core.PCACompute(imgMat, mean, eigenvector, imgMat.height )

    // output mean image
    val model_uri = getClass.getResource("/model/").getPath
    val mean_img = MatTool.convertMatFromOneDimensionMat(mean, size)
    Imgproc.resize(mean_img, mean_img, bunner_size)
    Highgui.imwrite(model_uri + "mean_img.bmp", mean_img)
    
    // output eigen image
    for(y <- 0 until eigenvector.height) {
      var eigen = new Mat
      Core.normalize(eigenvector.row(y), eigen, 0, 255, Core.NORM_MINMAX)
      val eigen_img = MatTool.convertMatFromOneDimensionMat(eigen, size)
      Imgproc.resize(eigen_img, eigen_img, bunner_size)
      Highgui.imwrite(model_uri+"eigen_"+y.toString+".bmp", eigen_img)
    }

    FileTools.writeCsv(mean, model_uri + "mean.csv")
    FileTools.writeCsv(eigenvector, model_uri + "eigenvector.csv")
 }
}


