package scalashop

import java.util.concurrent._
import scala.collection._
import org.junit._
import org.junit.Assert.assertEquals

class BlurSuite {

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)

  def printImg(img: Img): Unit = {
    for (y <- 0 until img.height) {
      for (x <- 0 until img.width) {
        print("(" + red(img(x, y)) + "," + green(img(x, y)) + "," + blue(img(x, y)) + "," + alpha(img(x, y)) + ")")
        print(" ")
      }
      println()
    }
  }

  def getTestImg(width: Int, height: Int) = {
    val testImg = new Img(width, height)
    for (y <- 0 until height) {
      for (x <- 0 until width) {
        testImg.update(x, y, rgba((x+1) * (y+1),(x+1) * (y+1),(x+1) * (y+1),(x+1) * (y+1)))
      }
    }
    testImg
  }

  @Test def `test boxBlurKernel`: Unit = {
    val testImg = getTestImg(3, 3)
    printImg(testImg)
    val pixel = boxBlurKernel(testImg, 1, 1, 1)
    println("(" + red(pixel) + "," + green(pixel) + "," + blue(pixel) + "," + alpha(pixel) + ")")
    assert(pixel == rgba(4, 4, 4, 4))
  }

  @Test def `boxBlurKernel should return the correct value on an interior pixel of a 3x4 image with radius 3`: Unit = {
    val testImg = getTestImg(3, 4)
    printImg(testImg)
    val pixel = boxBlurKernel(testImg, 1, 1, 3)
    println("(" + red(pixel) + "," + green(pixel) + "," + blue(pixel) + "," + alpha(pixel) + ")")
    assert(pixel == rgba(5, 5, 5, 5))
  }

  @Test def `test horizontal parBlur`: Unit = {
    val testImg = getTestImg(3, 3)
    println("testImg")
    printImg(testImg)
    var newImg = new Img(testImg.width, testImg.height)
    HorizontalBoxBlur.parBlur(testImg, newImg, 9999999, 1)
    println("newImg")
    printImg(newImg)
  }

  @Test def `test vertical parBlur`: Unit = {
    val testImg = getTestImg(3, 3)
    println("testImg")
    printImg(testImg)
    var newImg = new Img(testImg.width, testImg.height)
    VerticalBoxBlur.parBlur(testImg, newImg, 9999999, 1)
    println("newImg")
    printImg(newImg)
  }

  @Test def `VerticalBoxBlur.parBlur with 32 tasks should execute 32 parallel tasks for a 32x64 image, each blurring one strip`: Unit = {
    val testImg = getTestImg(32,64)
    var newImg = new Img(testImg.width, testImg.height)
    VerticalBoxBlur.parBlur(testImg, newImg, 32, 3)
  }
}
