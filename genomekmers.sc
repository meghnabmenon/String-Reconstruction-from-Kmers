//GENOME ASSEMBLY ACTIVITY

import scala.collection.mutable._
import scala.util.control.Breaks
object ibsgenome {
 //Stroring Genome in str1
   var str1="TTGAGTGCCAGCGAGTAGAGTTTTCTCTAAACTTTGCTTTTAAAGTTTAAGAAAAATGAGACACCTGCACCTC"
                                                  //> str1  : String = TTGAGTGCCAGCGAGTAGAGTTTTCTCTAAACTTTGCTTTTAAAGTTTAAGAAAAATGA
                                                  //| GACACCTGCACCTC
     var x=7                                      //> x  : Int = 7
     
   
   //Length of String
   print(str1.length-1)                           //> 72
   
   //Getting substring of str1
   str1.substring(10 , 13)                        //> res0: String = GCG
   var a=0                                        //> a  : Int = 0
 
  //ACTIVITY 1 - SPLITTING STRING INTO K-MERS
     
     //Creating an empty list a1
     var a1=ListBuffer.empty[String]              //> a1  : scala.collection.mutable.ListBuffer[String] = ListBuffer()
   
   //Appending k-mers to List a1
   for(a<-0 to str1.length-1)
   {
   if(a+x<=str1.length){
   a1:+=str1.substring(a , a+x)
   }
   }
    //Number of k-mers present in str1
    print(a1.length)                              //> 67
    
    //k-mers of Str1
    print(a1)                                     //> ListBuffer(TTGAGTG, TGAGTGC, GAGTGCC, AGTGCCA, GTGCCAG, TGCCAGC, GCCAGCG, CC
                                                  //| AGCGA, CAGCGAG, AGCGAGT, GCGAGTA, CGAGTAG, GAGTAGA, AGTAGAG, GTAGAGT, TAGAGT
                                                  //| T, AGAGTTT, GAGTTTT, AGTTTTC, GTTTTCT, TTTTCTC, TTTCTCT, TTCTCTA, TCTCTAA, C
                                                  //| TCTAAA, TCTAAAC, CTAAACT, TAAACTT, AAACTTT, AACTTTG, ACTTTGC, CTTTGCT, TTTGC
                                                  //| TT, TTGCTTT, TGCTTTT, GCTTTTA, CTTTTAA, TTTTAAA, TTTAAAG, TTAAAGT, TAAAGTT, 
                                                  //| AAAGTTT, AAGTTTA, AGTTTAA, GTTTAAG, TTTAAGA, TTAAGAA, TAAGAAA, AAGAAAA, AGAA
                                                  //| AAA, GAAAAAT, AAAAATG, AAAATGA, AAATGAG, AATGAGA, ATGAGAC, TGAGACA, GAGACAC,
                                                  //|  AGACACC, GACACCT, ACACCTG, CACCTGC, ACCTGCA, CCTGCAC, CTGCACC, TGCACCT, GCA
                                                  //| CCTC)
    
 var cc=a1(0)                                     //> cc  : String = TTGAGTG
    
    
    
    
       
  //ACTIVITY 2- ARRANGIMG K-MERS IN LEXICOGRAPHICAL ORDER
 
 //Sorting k-mers in Dictionary order and storing it in List b1
                   var i=0                        //> i  : Int = 0
                   var j=0                        //> j  : Int = 0
   var b1=a1                                      //> b1  : scala.collection.mutable.ListBuffer[String] = ListBuffer(TTGAGTG, TGAG
                                                  //| TGC, GAGTGCC, AGTGCCA, GTGCCAG, TGCCAGC, GCCAGCG, CCAGCGA, CAGCGAG, AGCGAGT,
                                                  //|  GCGAGTA, CGAGTAG, GAGTAGA, AGTAGAG, GTAGAGT, TAGAGTT, AGAGTTT, GAGTTTT, AGT
                                                  //| TTTC, GTTTTCT, TTTTCTC, TTTCTCT, TTCTCTA, TCTCTAA, CTCTAAA, TCTAAAC, CTAAACT
                                                  //| , TAAACTT, AAACTTT, AACTTTG, ACTTTGC, CTTTGCT, TTTGCTT, TTGCTTT, TGCTTTT, GC
                                                  //| TTTTA, CTTTTAA, TTTTAAA, TTTAAAG, TTAAAGT, TAAAGTT, AAAGTTT, AAGTTTA, AGTTTA
                                                  //| A, GTTTAAG, TTTAAGA, TTAAGAA, TAAGAAA, AAGAAAA, AGAAAAA, GAAAAAT, AAAAATG, A
                                                  //| AAATGA, AAATGAG, AATGAGA, ATGAGAC, TGAGACA, GAGACAC, AGACACC, GACACCT, ACACC
                                                  //| TG, CACCTGC, ACCTGCA, CCTGCAC, CTGCACC, TGCACCT, GCACCTC)
                                                  
  for( i <-0 to b1.length-2) {
            for (j <- i + 1 to b1.length-1) {
                if (b1(i).compareTo(b1(j))>0) {

                    // swap words[i] with words[j] using a temp variable
                    val temp = b1(i)
                    b1(i) = b1(j)
                    b1(j) = temp
                }
                
            }
        }
     
//Lexicographically arranged k-mers
      print(b1)                                   //> ListBuffer(AAAAATG, AAAATGA, AAACTTT, AAAGTTT, AAATGAG, AACTTTG, AAGAAAA, A
                                                  //| AGTTTA, AATGAGA, ACACCTG, ACCTGCA, ACTTTGC, AGAAAAA, AGACACC, AGAGTTT, AGCG
                                                  //| AGT, AGTAGAG, AGTGCCA, AGTTTAA, AGTTTTC, ATGAGAC, CACCTGC, CAGCGAG, CCAGCGA
                                                  //| , CCTGCAC, CGAGTAG, CTAAACT, CTCTAAA, CTGCACC, CTTTGCT, CTTTTAA, GAAAAAT, G
                                                  //| ACACCT, GAGACAC, GAGTAGA, GAGTGCC, GAGTTTT, GCACCTC, GCCAGCG, GCGAGTA, GCTT
                                                  //| TTA, GTAGAGT, GTGCCAG, GTTTAAG, GTTTTCT, TAAACTT, TAAAGTT, TAAGAAA, TAGAGTT
                                                  //| , TCTAAAC, TCTCTAA, TGAGACA, TGAGTGC, TGCACCT, TGCCAGC, TGCTTTT, TTAAAGT, T
                                                  //| TAAGAA, TTCTCTA, TTGAGTG, TTGCTTT, TTTAAAG, TTTAAGA, TTTCTCT, TTTGCTT, TTTT
                                                  //| AAA, TTTTCTC)
 //ACTIVITY 3- STRING RECONSTRUCTION FROM K-MERS
  
 
  var initial=cc                                  //> initial  : String = TTGAGTG
  var fin=cc                                      //> fin  : String = TTGAGTG
  var a2=0                                        //> a2  : Int = 0
 while(b1.length>=1){
 var z=0
  for(a2<-0 to b1.length-1){
  var inter=b1(a2)
                                                                         
  // concatenating the k-mers
   if(initial.substring(1,x) ==inter.substring(0,x-1) ){
   fin=fin+inter.substring(initial.length-1)
  initial=inter
  z=a2 }
  }
 b1.remove(z)
  }

// Initial String before Splitting into k-mers
print(str1)                                       //> TTGAGTGCCAGCGAGTAGAGTTTTCTCTAAACTTTGCTTTTAAAGTTTAAGAAAAATGAGACACCTGCACCTC

//Final String after concatinating k-mers
print(fin)                                        //> TTGAGTGCCAGCGAGTAGAGTTTTCTCTAAACTTTGCTTTTAAAGTTTAAGAAAAATGAGACACCTGCACCTC
str1.equals(fin)                                  //> res1: Boolean = true
 
}