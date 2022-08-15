library(knitr)



Reel.Generator<-function(blank,four,five,six,seven,pound,at,exclamation,wild,multiplier){
  
  reel.blank<-Symbol.Generator('B',blank)
  reel.four<-Symbol.Generator('4',four)
  reel.five<-Symbol.Generator('5',five)
  reel.six<-Symbol.Generator('6',six)
  reel.seven<-Symbol.Generator('7',seven)
  reel.pound<-Symbol.Generator('#',pound)
  reel.at<-Symbol.Generator('@',at)
  reel.exclamation<-Symbol.Generator('!',exclamation)
  reel.wild<-Symbol.Generator('W',wild)
  reel.multiplier<-Symbol.Generator('M',multiplier)
  
  reel<-rbind(reel.blank,reel.four)
  reel<-rbind(reel,reel.five)
  reel<-rbind(reel,reel.six)
  reel<-rbind(reel,reel.seven)
  reel<-rbind(reel,reel.pound)
  reel<-rbind(reel,reel.at)
  reel<-rbind(reel,reel.exclamation)
  reel<-rbind(reel,reel.wild)
  reel<-rbind(reel,reel.multiplier)
  reel<-na.omit(reel)
  return(reel)
}

Symbol.Generator<-function(symbol,size){
  if(size==0){
    reel.symbol<-matrix(NA,1)
  }else{
    reel.symbol<-matrix(symbol,size)
  }
  return(reel.symbol)
}

Slot.Generator<-function(){
  reel.1<-Reel.Generator(blank=0,four=3,five=3,six=2,seven=2,pound=4,
                         at=4,exclamation=3,wild=0,multiplier=0)
  reel.2<-Reel.Generator(blank=5,four=3,five=2,six=2,seven=2,pound=3,
                         at=3,exclamation=3,wild=1,multiplier=0)
  reel.3<-Reel.Generator(blank=27,four=12,five=12,six=12,seven=10,pound=12,
                         at=11,exclamation=11,wild=1,multiplier=1)
  reel.1<-sample(reel.1,size=length(reel.1),replace = FALSE)
  reel.2<-sample(reel.2,size=length(reel.2),replace = FALSE)
  reel.3<-sample(reel.3,size=length(reel.3),replace = FALSE)
  positions<-Spin(reel.1,reel.2,reel.3)
  
  slot.list<-list(reel.1,reel.2,reel.3,positions)
  names(slot.list)<-c("reel.1","reel.2","reel.3","positions")
  slot.list<-AdjustPosition(slot.list)
  return(slot.list)
}

Spin<-function(reel.1,reel.2,reel.3){
  position.1<-sample(1:length(reel.1),1)
  position.2<-sample(1:length(reel.2),1)
  position.3<-sample(1:length(reel.3),1)
  
  positions<-matrix(3,nrow=3,ncol=3)
  positions<-data.frame(positions)
  colnames(positions)<-c("reel.1","reel.2","reel.3")
  positions[1,1]<-position.1
  positions[1,2]<-position.2
  positions[1,3]<-position.3
  
  positions[2,1]<-position.1+1
  positions[2,2]<-position.2+1
  positions[2,3]<-position.3+1
  
  
  positions[3,1]<-position.1+2
  positions[3,2]<-position.2+2
  positions[3,3]<-position.3+2
  
  return(positions)
}



Slot.Spin<-function(slot.list){
  slot.list$positions<-Spin(slot.list$reel.1,slot.list$reel.2,slot.list$reel.3)
  slot.list<-AdjustPosition(slot.list)
  return(slot.list)
}

SlotDisplay<-function(slot.list){
  slot.disp<-c(slot.list$reel.1[slot.list$positions$reel.1[1]],
               slot.list$reel.1[slot.list$positions$reel.1[2]],
               slot.list$reel.1[slot.list$positions$reel.1[3]],
               slot.list$reel.2[slot.list$positions$reel.2[1]],
               slot.list$reel.2[slot.list$positions$reel.2[2]],
               slot.list$reel.2[slot.list$positions$reel.2[3]],
               slot.list$reel.3[slot.list$positions$reel.3[1]],
               slot.list$reel.3[slot.list$positions$reel.3[2]], 
               slot.list$reel.3[slot.list$positions$reel.3[3]]) 
  
  
  slot.matrix<-matrix(slot.disp, ncol=3, nrow=3)
  colnames(slot.matrix)<-c("Reel 1", "Reel 2", "Reel 3")
  return(slot.matrix)
}

AdjustPosition<-function(slot.list){
  slot.list$positions$reel.1<-ifelse(slot.list$positions$reel.1>length(slot.list$reel.1),
     slot.list$positions$reel.1-length(slot.list$reel.1),slot.list$positions$reel.1)
  
  slot.list$positions$reel.2<-ifelse(slot.list$positions$reel.2>length(slot.list$reel.2),
     slot.list$positions$reel.2-length(slot.list$reel.2),slot.list$positions$reel.2)
  
  slot.list$positions$reel.3<-ifelse(slot.list$positions$reel.3>length(slot.list$reel.3),
     slot.list$positions$reel.3-length(slot.list$reel.3),slot.list$positions$reel.3)
  return(slot.list)
}

PayOutCalc<-function(slot.line){
  s7.check<-length(grep("7|#|@|!|W",slot.line))
  seven.check<-length(grep("7|W",slot.line))
  four.check<-length(grep("4|W",slot.line))
  five.check<-length(grep("5|W",slot.line))
  six.check<-length(grep("6|W",slot.line))
  at.check<-length(grep("@|W",slot.line))
  pound.check<-length(grep("#|W",slot.line))
  exclamation.check<-length(grep("!|W",slot.line))
  
  if(slot.line[3]=="M"){
    symbol.m.check<-length(grep("#|@|!|W",slot.line))
    s7.m.check<-length(grep("7|#|@|!|W",slot.line))
    seven.m.check<-length(grep("7|W",slot.line))
    four.m.check<-length(grep("4|W",slot.line))
    five.m.check<-length(grep("5|W",slot.line))
    six.m.check<-length(grep("6|W",slot.line))
    at.m.check<-length(grep("@|W",slot.line))
    pound.m.check<-length(grep("#|W",slot.line))
    exclamation.m.check<-length(grep("!|W",slot.line))
  }else{
    s7.m.check<-0
    symbol.m.check<-0
    seven.m.check<-0
    four.m.check<-0
    five.m.check<-0
    six.m.check<-0
    at.m.check<-0
    pound.m.check<-0
    exclamation.m.check<-0
  }
  pay.checker<-data.frame(
    s7.m.check,
    s7.check,
    seven.check,
    four.check,
    five.check,
    six.check,
    at.check,
    pound.check,
    exclamation.check,
    symbol.m.check,
    seven.m.check,
    four.m.check,
    five.m.check,
    six.m.check,
    at.m.check,
    pound.m.check,
    exclamation.m.check
  )
  return(pay.checker)
}

PayAmount<-function(pay.checker,coins){
  if(coins==1){
    pay.active<-data.frame(two.s7=1,
                           two.7=2,
                           three.s7=3,
                           three.4=4,
                           three.5=5,
                           three.6=6,
                           three.7=20,
                           three.pound=7,
                           three.at=12,
                           three.exclamation=15,
                           one.symbol.m=1,
                           one.7.m=2,
                           two.s7.m=3,
                           two.4.m=4,
                           two.5.m=5,
                           two.6.m=6,
                           two.7.m=20,
                           two.pound.m=7,
                           two.at.m=12,
                           two.exclamation.m=15)
  }
  if(coins==2){
    pay.active<-data.frame(two.s7=1,
                           two.7=2,
                           three.s7=3,
                           three.4=4,
                           three.5=5,
                           three.6=6,
                           three.7=20,
                           three.pound=7,
                           three.at=12,
                           three.exclamation=15,
                           one.symbol.m=2,
                           one.7.m=4,
                           two.s7.m=6,
                           two.4.m=8,
                           two.5.m=10,
                           two.6.m=12,
                           two.7.m=40,
                           two.pound.m=14,
                           two.at.m=24,
                           two.exclamation.m=30)
  }
  if(coins==3){
    pay.active<-data.frame(two.s7=1,
                           two.7=2,
                           three.s7=3,
                           three.4=4,
                           three.5=5,
                           three.6=6,
                           three.7=20,
                           three.pound=7,
                           three.at=12,
                           three.exclamation=15,
                           one.symbol.m=4,
                           one.7.m=6,
                           two.s7.m=9,
                           two.4.m=12,
                           two.5.m=15,
                           two.6.m=18,
                           two.7.m=60,
                           two.pound.m=21,
                           two.at.m=36,
                           two.exclamation.m=45)
  }
  
  if(pay.checker$s7.check != 2){
    pay.active$two.s7=0
  }
  if(pay.checker$seven.check != 2){
    pay.active$two.7=0
  }
  if(pay.checker$s7.check != 3){
    pay.active$three.s7=0
  }
  if(pay.checker$four.check != 3){
    pay.active$three.4=0
  }
  if(pay.checker$five.check != 3){
    pay.active$three.5=0
  }
  if(pay.checker$six.check != 3){
    pay.active$three.6=0
  }
  if(pay.checker$seven.check != 3){
    pay.active$three.7=0
  }
  if(pay.checker$pound.check != 3){
    pay.active$three.pound=0
  }
  if(pay.checker$at.check != 3){
    pay.active$three.at=0
  }
  if(pay.checker$exclamation.check != 3){
    pay.active$three.exclamation=0
  }
  if(pay.checker$symbol.m.check != 1){
    pay.active$one.symbol.m=0
  }
  if(pay.checker$seven.m.check != 1){
    pay.active$one.7.m=0
  }
  if(pay.checker$s7.m.check != 2){
    pay.active$two.s7.m=0
  }
  if(pay.checker$four.m.check != 2){
    pay.active$two.4.m=0
  }
  if(pay.checker$five.m.check != 2){
    pay.active$two.5.m=0
  }
  if(pay.checker$six.m.check != 2){
    pay.active$two.6.m=0
  }
  if(pay.checker$seven.m.check != 2){
    pay.active$two.7.m=0
  }
  if(pay.checker$at.m.check != 2){
    pay.active$two.at.m=0
  }
  if(pay.checker$pound.m.check != 2){
    pay.active$two.pound.m=0
  }
  if(pay.checker$exclamation.m.check != 2){
    pay.active$two.exclamation.m=0
  }
  return(pay.active)
}

CoinPay<-function(slot.matrix,coin){
  payout<-rep(0,3)
  if(coin==1){
    pay.checker_1<-PayOutCalc(slot.matrix[2,])
    payout[2]<-max(PayAmount(pay.checker_1,1))
    return(payout)
  }
  if(coin==2){
    pay.checker_1<-PayOutCalc(slot.matrix[2,])
    payout[2]<-max(PayAmount(pay.checker_1,2))
    
    pay.checker_2<-PayOutCalc(slot.matrix[1,])
    payout[1]<-max(PayAmount(pay.checker_2,2))
    return(payout)
  }
  if(coin==3){
    pay.checker_1<-PayOutCalc(slot.matrix[2,])
    payout[2]<-max(PayAmount(pay.checker_1,3))
    
    pay.checker_2<-PayOutCalc(slot.matrix[1,])
    payout[1]<-max(PayAmount(pay.checker_2,3))
    
    pay.checker_3<-PayOutCalc(slot.matrix[3,])
    payout[3]<-max(PayAmount(pay.checker_3,3))
    return(payout)
  }
}

SlotMatrixDisplay<-function(slot.matrix,coins,pay){
  line.active<-matrix(FALSE,nrow=3,ncol=1)
  if(coins==1){
    line.active[2,1]<-TRUE
  }
  if(coins==2){
    line.active[1,1]<-TRUE
    line.active[2,1]<-TRUE
  }
  if(coins==3){
    line.active[1,1]<-TRUE
    line.active[2,1]<-TRUE
    line.active[3,1]<-TRUE
  }
  
  slot.matrix<-cbind(slot.matrix,line.active)
  slot.matrix<-as.data.frame(cbind(slot.matrix,pay))
  colnames(slot.matrix)[4]<-"Line Active"
  colnames(slot.matrix)[5]<-"Payout"
  print(kable(slot.matrix))
}



#the function PromptFunc takes in no variables. It prints a list of options
#for the user to choose from and takes a integer as the input. The function 
#outputs the users choice
PromptFunc<-function(){
  writeLines(#displaying prompt
    "Option 1: Output your credit balance
Option 2: Deposit more Credits
Option 3: Spin slot machine 
Option 4: Cash out
Option 5: Output Coins per spin amount
Option 6: Change Coins per spin")
  
  option<-as.integer(readline(prompt = "Enter option number: "))#asking 
  #user for input
  return(option)
}

#this function takes in the variables option which is a integer representing
#what the user wants to program to do.Output which is a list of length 3, the
#first variable being end_bool which is a integer, this variable is used to 
#break the while loop to stop the slot machine.The second variable is balance 
#which represents how many credits the user has. The third variable is 
#window.matrix whose rows represent the idexes corresponding to which section
#of that reel is being displayed
PromptOption<-function(option, output, slot.list){
  if(option==1){#if your option is to Output your credit balance
    cat("Your current credit balance is: ", output[[2]])
    cat("\n")
    return(output)
  }else if(option==2){#if your option is to deposit more credits
    deposit<-as.integer(#asking user for how many credits they want to deposit
      readline(prompt="Enter the amount(integers) of credits to deposit: "))
    output[[2]]<-output[[2]]+deposit
    cat("Your current credit balance is: ", output[[2]])
    cat("\n")
    return(output)
  }else if(option==3){#if your option is to spin the slot
    if(output[[2]]>=1){#this if statement is for if you have atleast 1 credit
      output[[3]]<-Slot.Spin(output[[3]])#Slot.Spin randomly spins the reels
      slot.matrix<-SlotDisplay(output[[3]])#a matrix of the window display of the slot machine
      pay<-CoinPay(slot.matrix,output[[4]])#calculating how much you won
      SlotMatrixDisplay(slot.matrix,output[[4]],pay)
      output[[2]]<-output[[2]]+sum(pay)-output[[4]]#account balance = accounnt balance+pay-1
      cat("You won: ",sum(pay))
      cat("\n")
      cat("Your current credit balance is: ",output[[2]])
      cat("\n")
      return(output)
    }else{#if you dont have sufficient credits
      print("Insufficient credits, deposit more")
      return(output)
    }
  }else if(option==4){#cashing out of the slot
    cat("Your cashing out with: ", output[[2]])
    cat("\n")
    output[[1]]<-1
    return(output)
  }else if(option==5){
    cat("Machine set to spin with ", output[[4]])
    cat(" coin")
    cat("\n")
    return(output)
  }else if(option==6){
    output[[4]]<-as.integer(#asking user for how many coins per spin
      readline(prompt="Enter the amount of coins you want to spin with between 1&3 : "))
    cat("You are spinning with  ", output[[2]])
    cat(" coins")
    cat("\n")
    return(output)
  }
  else{#improper option entered
    print("Improper option entered")
    cat("\n")
    return(output)
  }
}

#the function SlotManager takes in the variables reel.matrix which is a matrix 
#where the columns hold the stops of each reel. window.matrix is the matrix 
#where Each row of window.matrix corresponds with a reel, and the columns 
#correspond  to top, middle, and bottom of the display stops.
SlotManager<-function(slot.list){
  account_balance<-0 # setting account balance to 0
  slot.matrix<-SlotDisplay(slot.list)#creating the window
  #display of the slot machine
  print(slot.matrix)#printing window display of the slot machine
  
  output<-list(0, account_balance, slot.list, 1)#creating a list where the first 
  #element is used as a variable to stop the slot machine. The second variable
  #is the users account balance. The third variable is window matrix. The fourth variable 
  #is the amount of coins per spin
  names(output)<-c("End_bool", "balance", "slot.list", "Coin.Amount")
  
  
  while(output[[1]]==0){#this while loop will keep going until output[[1]], which
    #is the variable used to stop this loop, is equal to zero
    option<-PromptFunc()#asking the user for which option he wants
    output<-PromptOption(option, output, reel.matrix)#executing which option
    #the user wants
  }
}


