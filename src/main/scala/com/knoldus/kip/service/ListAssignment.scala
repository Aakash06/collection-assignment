package com.knoldus.kip.service

import com.knoldus.kip.RamDatabase
import com.knoldus.kip.models.{Gender,Marks, Student}

trait ListAssignment {

  //Assignment -1
  def failedStudents(subjectId: Long, percentage: Double, passOrFail: String): Int = {

    val passCount = RamDatabase.marksList.count(marks=>marks.subjectId==subjectId && marks.marksObtained >= percentage )

    val failCount = RamDatabase.marksList.count(marks=>marks.subjectId==subjectId && marks.marksObtained < percentage )

    if(passOrFail.toLowerCase=="pass")passCount else failCount
  }

  def topBottomStudents(subjectId: Long, count: Int, topOrBottom: String): List[Student] = {

    val StudentMarklist = RamDatabase.marksList.filter(_.subjectId==subjectId).sortBy(_.marksObtained)

    def listStudent(marks:List[Marks]):List[Student]= {

      for { mark <- marks
      student<- RamDatabase.studentList
        if student.id== mark.studentId
      } yield student
    }

  topOrBottom.toLowerCase match   {
    case "top"=>listStudent(StudentMarklist.reverse.take(count))
    case "bottom"=>listStudent(StudentMarklist.take(count))
   }
  }

  def topAndLeastScorers(topOrBottom: String, count: Int): List[Student] = {
    val totalMarks:List[Float]={
      for(student<- RamDatabase.studentList)
        yield RamDatabase.marksList.takeWhile(_.studentId==student.id).map(_.marksObtained).sum
    }

    val studentNameWithMarks=RamDatabase.studentList.zip(totalMarks).sortBy(_._2)
    topOrBottom.toLowerCase match {
      case "top"=>studentNameWithMarks.reverse.take(count).map(_._1)
      case "bottom"=>studentNameWithMarks.take(count).map(_._1)
    }
  }

  def getScholarshipGroups(percentage: Float, goodScholarship: Int, normalScholarship: Int)
  : (List[(Student, Int)], List[(Student, Int)]) = {
    val totalMarks:List[Float]={
      for(student<- RamDatabase.studentList)
        yield RamDatabase.marksList.takeWhile(_.studentId==student.id).map(_.marksObtained).sum
    }
    val percentages=totalMarks.map(_/5)
    val studentNameMorePercentage=RamDatabase.studentList.zip(percentages).filter(_._2>=percentage)
    val goodscholar=studentNameMorePercentage.unzip._1.map(x=>(x,goodScholarship))
    val studentNameLessPercentage=RamDatabase.studentList.zip(percentages).filter(_._2<percentage)
    val normalscholar=studentNameLessPercentage.unzip._1.map(x=>(x,normalScholarship))
    (goodscholar,normalscholar)
  }

  def passedOrFailed(passOrFail: String, percentage: Float): List[Student] = {
    val totalMarks:List[Float]={
      for(student<- RamDatabase.studentList)
        yield RamDatabase.marksList.takeWhile(_.studentId==student.id).map(_.marksObtained).sum
    }
    val percentages=totalMarks.map(_/5)

    passOrFail.toLowerCase match {
      case "pass"=>val studentNameMorePercentage=RamDatabase.studentList.zip(percentages).filter(_._2>=percentage)
                      studentNameMorePercentage.unzip._1
      case "fail"=>
        val studentNameLessPercentage=RamDatabase.studentList.zip(percentages).filter(_._2<percentage)
        studentNameLessPercentage.unzip._1
    }
}


  def studentsWithMoreThan95: List[Student] = {
    val totalMarks:List[Float]={
      for(student<- RamDatabase.studentList)
        yield RamDatabase.marksList.takeWhile(_.studentId==student.id).map(_.marksObtained).sum
    }
    val percentage=totalMarks.map(_/5)
    val studentNameWithMarks=RamDatabase.studentList.zip(percentage).filter(_._2>95)
    studentNameWithMarks.unzip._1
  }

  def generateReport: List[(String, List[Int])] = {
    val marksList = RamDatabase.marksList.sortBy(_.studentId)
    val studentsName = RamDatabase.studentList.map(_.name)
    val marks: List[List[Int]] = RamDatabase.studentList.map(x => marksList.groupBy(x.id == _.studentId)(true).map(_.marksObtained.toInt))
    studentsName.zip(marks)
  }

  //Assignment - 2
  def getLastElementWithIndex(list: List[String]): (String, Int) = {
    val list1= list.reverse
    (list1.head,list1.size-1)
  }

  def printTable(list: List[Long]): List[Long] = {
    val result=for{
      base<-0 to list.size
      toYield = for {
        index<- 1 to 10
      }yield list(base) * index
    }yield toYield.toList
    val result1=result.toList
    val result2=result1.flatMap(_ map(_*1))
    result2
  }

  def aggregateLists(list1: List[String], list2: List[Long]): List[String] = {
    val aggList= for {
      i<-0 until  list1.size-1
       finalList = list1(i) + " " + list2(i).toString
    }yield finalList
    aggList.toList
  }


  def getSumOfList(list: List[Long]): Long = {
    if(list.isEmpty)0 else list.head + getSumOfList(list.tail)
  }


  def getMultiplicationOfList(list: List[Long]) : Long = {
    if(list.isEmpty) 1 else list.head * getMultiplicationOfList(list.tail)
  }

  def quickSortList(list: List[Long]): List[Long] = {
    if (list.length <= 1)list else{
      val pivotElement = list(list.length / 2)
      quickSortList(list filter (_ < pivotElement )) ::: (list filter (_ == pivotElement )) ::: quickSortList(list filter(_ > pivotElement))
    }
  }

  def mergeSortList(list: List[Long]):List[Long] = {
    def merge(xs: List[Long], ys: List[Long]): List[Long] = {
      (xs, ys) match {
        case (Nil, _) => ys
        case (_, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (x < y) x :: merge(xs1, ys) else y :: merge(xs, ys1)
      }
    }

    val n = list.length / 2
        if (n == 0) list else {
          val (ys, zs) = list splitAt n
          merge(mergeSortList(ys), mergeSortList(zs))
        }
    }

}
