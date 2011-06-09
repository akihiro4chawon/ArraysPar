
import org.scalacheck.{Arbitrary, Prop}
import org.specs.{Specification, ScalaCheck, Sugar}
import java.util.Arrays
import java.lang.reflect.Method
import scala.util.Random

import akihiro.ParArrays

import org.scalacheck._
import org.scalacheck.Prop._

class SortTest extends Specification with Sugar with ScalaCheck {
  
  "sort" should {
    "sort array of" >> {
      "Byte"   >> checkSortOfType[Byte]
      "Char"   >> checkSortOfType[Char]
      "Double" >> checkSortOfType[Double]
      "Float"  >> checkSortOfType[Float]
      "Int"    >> checkSortOfType[Int]
      "Long"   >> checkSortOfType[Long]
      "Short"  >> checkSortOfType[Short]
      "Comparable" >> checkSortOfComparable
    }
    "sort the specified range of array of" >> {
      "Byte"   >> checkSortOfTypeWithIndex[Byte]
      "Char"   >> checkSortOfTypeWithIndex[Char]
      "Double" >> checkSortOfTypeWithIndex[Double]
      "Float"  >> checkSortOfTypeWithIndex[Float]
      "Int"    >> checkSortOfTypeWithIndex[Int]
      "Long"   >> checkSortOfTypeWithIndex[Long]
      "Short"  >> checkSortOfTypeWithIndex[Short]      
      "Comparable" >> checkSortOfComparableWithIndex
    }
  }
  
//  case class TestComparableObject(val key: String)
//    extends Ordered[TestComparableObject] {
//    def compare(that: TestComparableObject) =
//      this.key compare that.key
//  }
//  implicit def genTestComparable: Gen[TestComparableObject] =
//    for { s <- Gen.alphaStr } yield TestComparableObject(s)

  def checkSortOfComparable {
    Prop.forAll((a: Array[String]) => {
      val b = a.clone
      Arrays.sort(a.asInstanceOf[Array[java.lang.Object]])
      ParArrays.sort(b)
      Arrays.equals(a.asInstanceOf[Array[java.lang.Object]], b.asInstanceOf[Array[java.lang.Object]])
    }).label("checkSortOfComparable") must pass
    
    Prop.forAll((a: Array[String]) => {
      ParArrays.sort(a)
      val b = a.clone
      ParArrays.sort(b)
      Arrays.equals(a.asInstanceOf[Array[java.lang.Object]], b.asInstanceOf[Array[java.lang.Object]])
    }).label("checkIdempotentPropertyOfComparable") must pass    
  }
  
  def checkSortOfComparableWithIndex {
    Prop.forAll((a: Array[String]) => {
      val len = a.length
      val beg = if (len != 0) Random.nextInt(len) else 0
      val end = Random.nextInt(len - beg + 1) + beg
      val b = a.clone
      Arrays.sort(a.asInstanceOf[Array[java.lang.Object]])
      ParArrays.sort(b)
      Arrays.equals(a.asInstanceOf[Array[java.lang.Object]], b.asInstanceOf[Array[java.lang.Object]])
    }).label("checkSortOfComparable") must pass
  }
  
  def checkSortOfType[T](implicit
      m: ClassManifest[T],
      a: Arbitrary[Array[T]], s: Shrink[Array[T]], pp: Array[T] => Pretty) {
    Prop.forAll((a: Array[T]) => {
      val b = a.clone
      invokeArraysSort(a)
      invokeParArraysSort(b)
      invokeArraysEquals(a, b)
    }).label("checkSortOf"+m) must pass
    
    Prop.forAll((a: Array[T]) => {
      invokeParArraysSort(a)
      val b = a.clone
      invokeParArraysSort(a)
      invokeArraysEquals(a, b)
    }).label("checkIdempotentPropertyOf"+m) must pass
  }
  
  def checkSortOfTypeWithIndex[T] (implicit
      m: ClassManifest[T],
      a: Arbitrary[Array[T]], s: Shrink[Array[T]], pp: Array[T] => Pretty) {
    Prop.forAll((a: Array[Int]) => {
      val len = a.length
      val beg = if (len != 0) Random.nextInt(len) else 0
      val end = Random.nextInt(len - beg + 1) + beg
      val b = a.clone
      Arrays.sort(a, beg, end)
      ParArrays.sort(b, beg, end)
      Arrays.equals(a, b)
    }).label("checkSortOf"+m+"WithIndex") must pass    
  }  
  
  def invokeArraysSort[T : ClassManifest](a: Array[T]) {
    val arraysClass = java.lang.Class.forName("java.util.Arrays")
    val sortMethod = arraysClass.getMethod("sort",
        implicitly[ClassManifest[T]].arrayManifest.erasure)
    sortMethod.invoke(null, a)
  }
  
  def invokeParArraysSort[T: ClassManifest](a: Array[T]) {
    val parArraysClass = java.lang.Class.forName("akihiro.ParArrays")
    val sortMethod = parArraysClass.getMethod("sort",
        implicitly[ClassManifest[T]].arrayManifest.erasure)
    sortMethod.invoke(null, a)    
  }
  
  def invokeArraysEquals[T: ClassManifest](a: Array[T], b: Array[T]): Boolean = {
    val argClass = implicitly[ClassManifest[T]].arrayManifest.erasure
    val arraysClass = java.lang.Class.forName("java.util.Arrays")
    val equalsMethod = arraysClass.getMethod("equals", argClass, argClass)
    equalsMethod.invoke(null, a, b).asInstanceOf[Boolean]
  }
}

