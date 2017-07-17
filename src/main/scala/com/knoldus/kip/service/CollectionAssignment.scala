package com.knoldus.kip.service

import com.knoldus.kip.RamDatabase
import com.knoldus.kip.models.{Gender, Marks, ScoreCard, Student}

trait CollectionAssignment {

  def computeScoreCard(id:Long):ScoreCard={

    val marksLists:List[Marks]= RamDatabase.marksList.filter(_.studentId==id)

    val marks:List[Float]=marksLists.map(_.marksObtained)

    val subjectList :List[Long]= marksLists.map(_.subjectId.toLong)

    val totalSubject= subjectList.size

    val map:Map[Long,Float]=subjectList zip marks toMap

    val percentage= marks.sum / totalSubject

    ScoreCard(id,map,percentage)

  }
  //Collection Based - Assignment 1
  def generateScorecards: Map[String, AnyRef] = {
    val names: List[String] = RamDatabase.studentList.map(_.name)

    val scorecards: List[ScoreCard] = for{ s <- RamDatabase.studentList}
      yield computeScoreCard(s.id)

    names zip scorecards toMap

  }

  def getScorecardsByName(name: String): List[ScoreCard] = {

    val studentList:List[Student]=RamDatabase.studentList.filter(_.name==name).sortBy(_.id)

    val scoreCards:List[ScoreCard]= for{x <- studentList} yield computeScoreCard(x.id)

    if(scoreCards.isEmpty) throw new Exception("No data Found") else scoreCards
  }

  //Collection Based - Assignment 2
  def getScoreCardByGender: (List[ScoreCard], List[ScoreCard]) = {
    val maleStudentList= RamDatabase.studentList.filter(_.gender== Gender.MALE).sortBy(_.id)

    val femaleStudentList= RamDatabase.studentList.filter(_.gender== Gender.FEMALE).sortBy(_.id)

    val maleScoreCards:List[ScoreCard]= for{x <- maleStudentList} yield computeScoreCard(x.id)

    val femalScoreCards:List[ScoreCard]= for{x <- femaleStudentList} yield computeScoreCard(x.id)

    (maleScoreCards,femalScoreCards)
  }

  def getScoreCardsWithHigherPercentage: (List[ScoreCard], List[ScoreCard]) = {
  val (maleList,femaleList)=getScoreCardByGender
    (maleList.filter(_.percentage>=50),femaleList.filter(_.percentage>=50))


  } //Internally calls getScoreCardByGender

  def getSimilarPercentageBwGroups: List[((String, ScoreCard), (String, ScoreCard))] = {
    val t:(List[ScoreCard], List[ScoreCard]) = getScoreCardByGender
    val maleScorecard: List[ScoreCard] = t._1
    val femaleScorecard: List[ScoreCard] = t._2

    val selectedFemales: List[ScoreCard] =for{f <- femaleScorecard
                                              m <- maleScorecard
                                              if f.percentage == m.percentage
    }yield f
    val femaleNames: List[String] = selectedFemales.flatMap(x => RamDatabase.studentList.filter(x.studentId == _.id).map(_.name))

    val selectedMales: List[ScoreCard] =for{m <- maleScorecard
                                            f <- femaleScorecard
                                            if f.percentage == m.percentage
    }yield m
    val maleNames: List[String] = selectedMales.flatMap(x => RamDatabase.studentList.filter(x.studentId == _.id).map(_.name))

    val list: List[((String, ScoreCard), (String, ScoreCard))] = maleNames.zip(selectedMales).zip(femaleNames.zip(selectedFemales))
    list

  }

  def femalePercentageNotInMales: List[(String, ScoreCard)] = {
    val t:(List[ScoreCard], List[ScoreCard]) = getScoreCardByGender

    val maleScorecard: List[ScoreCard] = t._1
    val femaleScorecard: List[ScoreCard] = t._2

    val selectedFemales: List[ScoreCard] =for{f <- femaleScorecard
                                              m <- maleScorecard
                                              if f.percentage != m.percentage
    }yield f
    val names: List[String] = selectedFemales.flatMap(x => RamDatabase.studentList.filter(x.studentId == _.id).map(_.name))
    names.zip(selectedFemales)
  }

}
