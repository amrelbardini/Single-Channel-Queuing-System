let randomArr=[];
const critValue=0.136;
let correlationFlag=0;
let chiSquareFlag=0;
let ksFlag=0;
let expFlag=0;
let segmentsArr=[0,0,0,0,0,0,0,0,0,0]; //observed
let expectedArr=[10,10,10,10,10,10,10,10,10,10];
// generate 100 random number
for(let i=0;i<100;i++){
    randomArr[i]=Math.random();
}
//console.log(randomArr);
// divide generated numbers to 10 segments 
for(let i=0; i<randomArr.length;i++){
    if(randomArr[i]<0.1){
        segmentsArr[0]=segmentsArr[0]+1;
    }else if( randomArr[i]>0.1 && randomArr[i]<0.2){
        segmentsArr[1]+=1;
    }else if( randomArr[i]>0.2 && randomArr[i]<0.3){
        segmentsArr[2]+=1;
    }else if( randomArr[i]>0.3 && randomArr[i]<0.4){
        segmentsArr[3]+=1;
    }else if( randomArr[i]>0.4 && randomArr[i]<0.5){
        segmentsArr[4]+=1;
    }else if( randomArr[i]>0.5 && randomArr[i]<0.6){
        segmentsArr[5]+=1;
    }else if( randomArr[i]>0.6 && randomArr[i]<0.7){
        segmentsArr[6]+=1;
    }else if( randomArr[i]>0.7 && randomArr[i]<0.8){
        segmentsArr[7]+=1;
    }else if( randomArr[i]>0.8 && randomArr[i]<0.9){
        segmentsArr[8]+=1;
    }else if( randomArr[i]>0.9 && randomArr[i]<1.0){
        segmentsArr[9]+=1;
    }   
}
console.log(segmentsArr);

// implement chi-square test 
function singleChiSquareTerm(observedValue,expectedValue){
    return Math.pow(observedValue - expectedValue, 2) / expectedValue;
}
function calculateChiSquaredStatistic(observations,expectations){
    let resultSet={
        chiSquared:0,
        terms:[],
    }
    let N=observations.length;
    for(let i=0;i<N;i++){
        let singleTerm=singleChiSquareTerm(observations[i],expectations[i]);
        resultSet.terms.push(singleTerm);
        resultSet.chiSquared+=singleTerm;
    }
    return resultSet;
}

let ChiSquaredStatistic=calculateChiSquaredStatistic(segmentsArr,expectedArr).chiSquared;
console.log(ChiSquaredStatistic);

// at alpha=0.05 degreeOf freedom=outcome-1=9, we get critical value=16.919
const CRITICALVALUE=16.919;
if(ChiSquaredStatistic<=CRITICALVALUE){
    console.log("H0:UNIFORM & ACCEPTED!");
    chiSquareFlag=1;
}else{
    console.log("H1:RN NOT UNIFORM & REJECTED!");
    chiSquareFlag=0;
}

// implement kolmogorov test 

function plusFormula(n,i,value){
    if (value < 0) {
        value = 0;
    } else if (value >= 1) {
        value = 1;
    }
    let div=i/n;
    //console.log("Minus value: "+ (div-value));
    return div-value;
}
function minusFormula(n,i,value){
    if (value < 0) {
        value = 0;
    } else if (value >= 1) {
        value = 1;
    }
    let div = (i-1.0)/n;
    //console.log("Minus value: "+ (value-div));
    return value-div;
}

function kolmogorovSmirnov(values){
   let dplus=0;
   let dMinus=0;
   let plus=[];
   let minus=[];
   for(let i=0;i<values.length;i++){
       dplus=plusFormula(values.length,i,values[i]);
       plus.push(dplus);

       dMinus=minusFormula(values.length,i,values[i]);
       minus.push(dMinus);
   }
   for (let i = 0; i < values.length; i++) {
    if (plus[i] > dplus) {
        dplus = plus[i];
    }
    if (minus[i] > dMinus) {
        dMinus = minus[i];
    }
    }
    return Math.max(dMinus,dplus);
}
//input for kolmogorov segment array over N 
let sortedRN=randomArr.sort();

let outcome = kolmogorovSmirnov(sortedRN);
// aplha=0.05 from table critical value is equal to 0.136 
console.log("outcome of kolmogorov "+outcome);

if( outcome<=critValue){
    ksFlag=1;
    console.log("H0 : UNIFORM AND ACCEPTED! " + outcome);
}else{
    console.log("H1 : NOT UNIFORM AND REJECTED!" + outcome);
    ksFlag=0;
}
// auto correlation test 
let i=1;
let M;
let m=5;
let rho,sigma,z;
function Summation(values){
    let sum = 0;
    let m=5;
        for (let j = ((i) + m); j < values.length; j += m)
            sum += (values[j - m] * values[j]);
        return sum;
}
function printResult(values){
   console.log("Result -");
   console.log("M = " + M);
   console.log("Summation = " + Summation(values));
   console.log("Rho = " + rho);
   console.log("Sigma = " + sigma);
   console.log("Z = " + z);
}
function autoCorrelation(values){
    //perform test
    let N=values.length;
    M=((N-i)/m)-1;
    rho=((1.0/(M+1)) * Summation(values))-0.25;
    sigma=Math.sqrt(13*M+7)/(12*(M+1));
    z=rho/sigma;
    printResult(values);
    return z;
}
let testResult= autoCorrelation(randomArr);
// taking alpha =0.05  Z of o.o25----> c.v from -1.96 to + 1.96
if(testResult>=-1.96 && testResult<=1.96){
    console.log("z-static lies in the acceptance region, there's no correlation between mentioned numbers");
    correlationFlag=1;
}else{
    correlationFlag=0;
    console.log("z-static lies in the  outside acceptance region, there's  correlation between mentioned numbers TEST FAILED!");
}
// assuming lambda=1, x is the random input, x>=0 no all negative values are converted to 0
function exponentialDistribution(values){
    let expArray=[];
    for(let i=0;i<values.length;i++){
        if(values[i]>=0){
            expArray.push((1)-Math.exp(-values[i]/1)); 
            // expArray.push(1-Math.exp(-values[i]); 
         }else{
             expArray.push(0);
         }
    }
   console.log(expArray);
    return expArray;
}
let expDistributedArray=exponentialDistribution(randomArr);
// testing should fail due to different distribution
let sortedExpDistrubtedArr=expDistributedArray.sort();

let DisOutcome=kolmogorovSmirnov(sortedExpDistrubtedArr);

if(DisOutcome>critValue){
    console.log("Exponential Distribution Outcome is not UNIFORM "+DisOutcome);
    expFlag=1;
}else{
    console.log("Exponential Distribution Outcome is  UNIFORM  check conversion"+DisOutcome);
}
//start graphs code 
const labels = [
    '0-0.1',
    '0.1-0.2',
    '0.2-0.3',
    '0.3-0.4',
    '0.4-0.5',
    '0.5-0.6',
    '0.6-0.7',
    '0.7-0.8',
    '0.8-0.9',
    '0.9-1.0',
  ];

  const data = {
    labels: labels,
    datasets: [{
      label: 'Chi-Squared Test',
      backgroundColor: 'rgb(255, 99, 132)',
      borderColor: 'rgb(255, 99, 132)',
      data: segmentsArr,
    }]
  };

  const config = {
    type: 'bar',
    data: data,
    options: {
      scales: {
        y: {
          beginAtZero: true
        }
      }
    },
  };

// render graph 
const myChart = new Chart(
    document.getElementById('myChart'),
    config
  );
  //current RN distribution- GRAPH 
  const data1 = {
    labels:labels ,
    datasets: [{
      label: 'Generated RN-Distribution',
      backgroundColor: 'rgb(255, 99, 132)',
      borderColor: 'rgb(255, 99, 132)',
      data:segmentsArr ,
    }]
  };
  const config1 = {
    type: 'line',
    data: data1, 
    options: {
        indexAxis: 'x',
        scales: {
          x: {
            beginAtZero: false
          },
          y: {
            beginAtZero: false
          }
        }
      }
  };
  const myChart2 = new Chart(
    document.getElementById('myChart2'),
    config1
  );
  // exponential graph
  let expDistributionSegments=[0,0,0,0,0,0,0,0,0,0];
  for(let i=0;i<expDistributedArray.length;i++){
    if(expDistributedArray[i]<0.1){
        expDistributionSegments[0]+=1;
    }else if( expDistributedArray[i]>0.1 && expDistributedArray[i]<0.2){
        expDistributionSegments[1]+=1;
    }else if( expDistributedArray[i]>0.2 && expDistributedArray[i]<0.3){
        expDistributionSegments[2]+=1;
    }else if( expDistributedArray[i]>0.3 && expDistributedArray[i]<0.4){
        expDistributionSegments[3]+=1;
    }else if( expDistributedArray[i]>0.4 && expDistributedArray[i]<0.5){
        expDistributionSegments[4]+=1;
    }else if( expDistributedArray[i]>0.5 && expDistributedArray[i]<0.6){
        expDistributionSegments[5]+=1;
    }else if( expDistributedArray[i]>0.6 && expDistributedArray[i]<0.7){
        expDistributionSegments[6]+=1;
    }else if( expDistributedArray[i]>0.7 && expDistributedArray[i]<0.8){
        expDistributionSegments[7]+=1;
    }else if( expDistributedArray[i]>0.8 && expDistributedArray[i]<0.9){
        expDistributionSegments[8]+=1;
    }else if( expDistributedArray[i]>0.9 && expDistributedArray[i]<1.0){
        expDistributionSegments[9]+=1;
    }   
  }
  console.log(expDistributionSegments);

  const data3 = {
    labels:labels ,
    datasets: [{
      label: 'Exponential curve',
      backgroundColor: 'rgb(255, 99, 132)',
      borderColor: 'rgb(255, 99, 132)',
      data:expDistributionSegments,
    }]
  };
  const config3 = {
    type: 'line',
    data: data3,
    options: {
      indexAxis: 'x',
      scales: {
        x: {
          beginAtZero: false
        }
      }
    }
  };
  const myChart3 = new Chart(
    document.getElementById('myChart3'),
    config3
  );
  // KS graph
const labels4=[
    "0.00-0.01",
    "0.01-0.02",
    "0.02-0.03",
    "0.03-0.04",
    "0.04-0.05",
    "0.05-0.06",
    "0.06-0.07",
    "0.07-0.08",
    "0.08-0.09",
    "0.09-0.10",
    "0.10-0.11",
    "0.11-0.12",
    "0.12-0.13",
    "0.13-0.14",
    "0.14-0.15",
    "0.15-0.16",
    "0.16-0.17",
    "0.17-0.18",
    "0.18-0.19",
    "0.19-0.20",
    "0.21-0.22",
    "0.22-0.23",
    "0.23-0.24",
    "0.24-0.25",
    "0.25-0.26",
    "0.26-0.27",
    "0.27-0.28",
    "0.28-0.29",
    "0.29-0.30",
    "0.30-0.31",
    "0.31-0.32",
    "0.32-0.33",
    "0.33-0.34",
    "0.34-0.35",
    "0.35-0.36",
    "0.36-0.37",
    "0.37-0.38",
    "0.38-0.39",
    "0.39-0.40",
    "0.40-0.41",
    "0.41-0.42",
    "0.42-0.43",
    "0.43-0.44",
    "0.44-0.45",
    "0.45-0.46",
    "0.46-0.47",
    "0.47-0.48",
    "0.49-0.50",
    "0.50-0.51",
    "0.51-0.52",
    "0.52-0.53",
    "0.53-0.54",
    "0.54-0.55",
    "0.55-0.56",
    "0.57-0.58",
    "0.58-0.59",
    "0.59-0.60",
    "0.60-0.61",
    "0.61-0.62",
    "0.62-0.63",
    "0.63-0.64",
    "0.64-0.65",
    "0.65-0.66",
    "0.66-0.67",
    "0.67-0.68",
    "0.68-0.69",
    "0.69-0.70",
    "0.70-0.71",
    "0.71-0.72",
    "0.72-0.73",
    "0.73-0.74",
    "0.74-0.75",
    "0.75-0.76",
    "0.76-0.77",
    "0.77-0.78",
    "0.78-0.79",
    "0.79-0.80",
    "0.80-0.81",
    "0.81-0.82",
    "0.82-0.83",
    "0.83-0.84",
    "0.84-0.85",
    "0.85-0.86",
    "0.86-0.87",
    "0.87-0.88",
    "0.88-0.89",
    "0.89-0.90",
    "0.90-0.91",
    "0.91-0.92",
    "0.92-0.93",
    "0.93-0.94",
    "0.94-0.95",
    "0.95-0.96",
    "0.96-0.97",
    "0.97-0.98",
    "0.99-1.00",
];
  const data4 = {
    labels:labels4 ,
    datasets: [{
      label: 'KS curve',
      backgroundColor: 'rgb(255, 99, 132)',
      borderColor: 'rgb(255, 99, 132)',
      data:randomArr,
    }]
  };
  const config4 = {
    type: 'line',
    data: data4,
    options: {
      indexAxis: 'x',
      scales: {
        x: {
          beginAtZero: true
        }
      }
    }
  };
  const myChart4 = new Chart(
    document.getElementById('myChart4'),
    config4
  );

//start solving the problem & exponential distribution 
let interArrivalExpArray=[];
let serviceTimeUniformArray=[];
if(chiSquareFlag==1 && ksFlag==1 && correlationFlag==1 && expFlag==1 ){
   console.log("ALL TESTS PASSED RN PATCH ACCEPTED");
   document.getElementById('teststatHolder').classList.add('pass');
   for(let i=0;i<expDistributedArray.length;i++){
       interArrivalExpArray[i]=expDistributedArray[i]*100;
       serviceTimeUniformArray[i]=randomArr[i]*100;
   }
   console.log("exp interarrival array ");
   console.log( interArrivalExpArray);
//    console.log("uniform service array "+ serviceTimeUniformArray);
}else{
    document.getElementById('teststatHolder').classList.remove('pass');
    document.getElementById('teststatHolder').classList.add('fail');
    console.log("PATCH REJECTED TEST FAILED! Reload for a new patch!");
}

// //system time depends on events 
// //random digits indicate time arrival for each customer Math.random() generates values from 0 to 1 but never 1
// //simulation will run for 20 customers

let serviceTime;  //random service time from tables
let interarrivalTime; // random arrival time from tables
let arrivalTime=[]; // arrival time after checking the ranges array
let cumulativeArrivalProb=[];
let endTime=[];
let idleTime=[];
let timeServiceBegins=[];
let timeCustomerWait=[];
let timeCustomerSpends=[];
let arrivalOnClock=[];
let serviceTimeArr=[];
let customerNum=0;
let Arrcounter=0;
let Servcounter=0;
//generate dynamic arrival range
let ArrivalRanges=[];
let ServiceRanges=[];
//static inputs 
let arrivalCounter=0;
let serviceCounter=0;
// let interArrivalStaticArray=[10,72,53,4,29,54,92,16,33,71];
// let serviceStaticArray=[89,76,81,55,96,86,63,36,58,9];

// arrival time calculations 
let timeBetArrivals=[10,11,12,13,14,15];
console.log("time bet arrivals : "+timeBetArrivals);
let probability=[0.08,0.18,0.30,0.20,0.19,0.05];
for(let i=0;i<probability.length;i++){
    cumulativeArrivalProb[i]=probability[i];   
}
for(let j=1;j<probability.length;j++){
    cumulativeArrivalProb[j]+=cumulativeArrivalProb[j-1];
}
console.log( "cumulative Arrival Probability : "+cumulativeArrivalProb);
//generate arrival ranges from cumulative probability cumulativeArrivalProb x 100
for(let i=0;i<cumulativeArrivalProb.length;i++){
    ArrivalRanges[i]=Math.floor(cumulativeArrivalProb[i]*100);
}
console.log("Arrival ranges  "+ ArrivalRanges);

//service time prob calculations 
let serviceTimeProbability=[0.05,0.10,0.15,0.45,0.25];
let serviceCumulativeProb=[];
for(let i=0;i<serviceTimeProbability.length;i++){
    serviceCumulativeProb[i]=serviceTimeProbability[i];   
}
for(let j=1;j<serviceTimeProbability.length;j++){
    serviceCumulativeProb[j]+=serviceCumulativeProb[j-1];
}
console.log( "service time cumulative probabilty : "+serviceCumulativeProb);
// generate service time ranges 
for(let i=0;i<serviceCumulativeProb.length;i++){
    ServiceRanges[i]=Math.floor(serviceCumulativeProb[i]*100);
}
console.log("service ranges  "+ ServiceRanges);
//start table A
function arrTime(){   
//start static adjustment 
let interarrivalTime=interArrivalExpArray[arrivalCounter];

if(interarrivalTime>0.0 && interarrivalTime<=ArrivalRanges[0]){
    interarrivalTime=10000;
    console.log("arrival range---> 1-8 ",  interarrivalTime+" ms"); 
}else if(interarrivalTime>ArrivalRanges[0] && interarrivalTime<=ArrivalRanges[1]){
    interarrivalTime=11000;
    console.log("arrival range---> 8-26 ",  interarrivalTime+" ms");
}else if(interarrivalTime>ArrivalRanges[1] && interarrivalTime<=ArrivalRanges[2]){
    interarrivalTime=12000;
    console.log("arrival range---> 26-56 ", interarrivalTime+" ms");
}else if(interarrivalTime>ArrivalRanges[2] && interarrivalTime<=ArrivalRanges[3]){
    interarrivalTime=13000;
    console.log("arrival range---> 56-76 ", interarrivalTime+" ms");
}else if(interarrivalTime>ArrivalRanges[3] && interarrivalTime<=ArrivalRanges[4]){
    interarrivalTime=14000;
    console.log("arrival range---> 76-95", interarrivalTime+" ms");
}else if(interarrivalTime>ArrivalRanges[4] ){
   // && interarrivalTime<=ArrivalRanges[5]
    interarrivalTime=15000;
    console.log("arrival range---> 95-100", interarrivalTime+" ms");
}  
   //add interarrival time values to arivalTime array & increment counter 
   arrivalTime[Arrcounter]=interarrivalTime/1000;
   Arrcounter++;
   console.log("Arrival time Table "+arrivalTime);
   arrivalCounter++;
    return interarrivalTime;
}

 //start service time table B

//end tables code
function servTime(){
//start static service time 
let serviceTime=serviceTimeUniformArray[serviceCounter];
if(serviceTime>0.0 && serviceTime<=ServiceRanges[0]){
    serviceTime=15000;
    console.log("service range---> 1-5 ",  serviceTime+" ms"); 
}else if(serviceTime>ServiceRanges[0] && serviceTime<=ServiceRanges[1]){
    serviceTime=16000;
    console.log("service  range---> 5-15 ",  serviceTime+" ms");
}else if(serviceTime>ServiceRanges[1] && serviceTime<=ServiceRanges[2]){
    serviceTime=17000;
    console.log("service range---> 15-30 ",  serviceTime+" ms");
}else if(serviceTime>ServiceRanges[2] && serviceTime<=ServiceRanges[3]){
    serviceTime=18000;
    console.log("service range---> 30-75 ",  serviceTime+" ms");
}else if(serviceTime>ServiceRanges[3] && serviceTime<=ServiceRanges[4]){
    serviceTime=19000;
    console.log("service range---> 75-100 ",  serviceTime+" ms");
}
   //add interarrival time values to arivalTime array & increment counter 
   serviceTimeArr[Servcounter]=serviceTime/1000;
   Servcounter++;
   serviceCounter++;
   console.log("Service Time Table "+serviceTimeArr);
    return serviceTime;
}
//arrival on the clock calculation 
function getArrivalTime(){
    //check arrivalTime length 
    let j=0;
    if(customerNum===0){
        arrivalOnClock[0]=customerNum;
    }else{
        for(let i=1;i<arrivalTime.length;i++){
            arrivalOnClock[i]=arrivalTime[i]+arrivalOnClock[j];
            j++;
        }
    }
    console.log(arrivalOnClock);
    return arrivalOnClock;
}
function fillTableVals(){
        for (i = 0; i <= customerNum; i++) {
            if (i == 0) { // first service
                endTime[i] = serviceTimeArr[i];
                timeCustomerSpends[i] = serviceTimeArr[i];
                idleTime[i]=0;
                timeCustomerWait[i]=0;
                timeServiceBegins[i]=arrivalOnClock[0];
            } else { // next services
                if (endTime[i - 1] >= arrivalOnClock[i]) { // customer waiting
                    timeServiceBegins[i] = endTime[i - 1];
                    timeCustomerWait[i] = endTime[i - 1] - arrivalOnClock[i];
                    timeCustomerSpends[i] = serviceTimeArr[i] + timeCustomerWait[i];
                    idleTime[i]=0;
                } else { // server idle
                    idleTime[i] = arrivalOnClock[i] - endTime[i - 1];
                    timeServiceBegins[i] = arrivalOnClock[i];
                    timeCustomerSpends[i] = serviceTimeArr[i];
                    timeCustomerWait[i]=0;
                }
                endTime[i] = timeServiceBegins[i] + serviceTimeArr[i]; // calculate end time after service
            }
        }
    console.log("time service ends "+endTime);
    console.log("timeCustomerSpends "+timeCustomerSpends);
    console.log("timeServiceBegins "+timeServiceBegins);
    console.log("time customer wait  "+timeCustomerWait);
    console.log("idleTime "+idleTime);
}
//**************************************** UI CODE *****************************************************//

//start button 
let startBtn=document.getElementById('start');
startBtn.addEventListener('click',function(){
    for(let i=0;i<100;i++){
        if(customerNum<100 && chiSquareFlag==1 && ksFlag==1 && correlationFlag==1 && expFlag==1){
            serviceTime=servTime();
            interarrivalTime=arrTime();
            setTimeout(arrival,interarrivalTime);
            getArrivalTime();
            fillTableVals();
            addElemToTable(); 
            AvgWaitingTime(); 
            probabilityWait();
            probabilityIdleServer();
            avgServiceTime(); 
            averageTimeBetArrivals();
            AvgWaitingOfThoseWhoWait();
            avgTimeCustomerSpends(); 
        }else if(customerNum>=100){
            document.getElementById('limit').classList.add('show');
        }
    }     
});

//reset button 
let resetBtn=document.getElementById('reset');
resetBtn.addEventListener('click',function(){
    window.location.reload();  
});

//departed customers 
let departedCustomers=parseInt(document.getElementById('departed').innerText);
//customers in queue
let queue=document.getElementById('queue');
console.log('current customers in queue '+queue.innerText);

//join queue (event)
let customersInQueue=parseInt(document.getElementById('queue').innerText);
function joinQueue(customersInQueue){
         customersInQueue=parseInt(document.getElementById('queue').innerText)+1;
         queue.innerText=customersInQueue;
         return true;
};
// calculate average waiting time
function AvgWaitingTime(){
    let totalCustomerWait=0;
    for(let i=0;i<timeCustomerWait.length;i++){
    totalCustomerWait+=timeCustomerWait[i];
    }
    console.log("totalCustomerWait "+totalCustomerWait);
    console.log("average waiting time  "+totalCustomerWait/customerNum);  
    document.getElementById('avgWaitTime').innerText=totalCustomerWait/customerNum;
}
// probability wait 
function probabilityWait(){
    let waitingCustomers=0;
    for(i=0;i<timeCustomerWait.length;i++){
        if(timeCustomerWait[i]>0){
            waitingCustomers++;
        }
    }
    console.log("probability wait "+waitingCustomers/customerNum);
}
// probability of idle server 
function probabilityIdleServer(){
    let totalIdleTime=0;
    let totalEndTime=0;
    for(i=0;i<idleTime.length;i++){
        if(idleTime[i]>0){
            totalIdleTime+=idleTime[i];
        }
    }
    for(let i=0;i<endTime.length;i++){
        totalEndTime+=endTime[i];
    }
    console.log("probability of idle server "+totalIdleTime/totalEndTime);
}
//average service time 
function avgServiceTime(){
    let totalServiceTime=0;
    for(let i=0;i<serviceTimeArr.length;i++){
        totalServiceTime+=serviceTimeArr[i];
    }
  console.log("average service time " +totalServiceTime/customerNum);
};
//average time between arrivals 
function averageTimeBetArrivals(){
    //Recheck Logic!!!!!
    let totalTimeBetArrivals=0;
    for(let i=0;i<arrivalTime.length;i++){
        totalTimeBetArrivals+=arrivalTime[i];
    }
    console.log("average time between arrivals "+ totalTimeBetArrivals/customerNum);
}
// average waiting time of those who wait 
function AvgWaitingOfThoseWhoWait(){
    let totalCustomersWhoWait=0;
    let customersWaitTime=0;
    for(let i=0;i<timeCustomerWait.length;i++){ 
        customersWaitTime+=timeCustomerWait[i];
        if(timeCustomerWait[i]>0){
            totalCustomersWhoWait++;
           }
     } 
    console.log("average waiting time of those who wait  "+customersWaitTime/totalCustomersWhoWait);  
}
// average time customer spends in the system 
function avgTimeCustomerSpends(){
    let totalSpentTime=0;
    for(let i=0; i<timeCustomerSpends.length;i++){
        totalSpentTime+=timeCustomerSpends[i];
    }
    console.log("average time customer spends in the system  "+ totalSpentTime/customerNum);
}

function beginService(){
    setTimeout(changeServStat,serviceTime); 
}

function changeServStat(){
    let serverStat=document.getElementById('server-status').innerText.toLocaleLowerCase();
    let currentCustomers=parseInt(document.getElementById('customers-in-system').innerText); 
    if(serverStat==='idle'&& customersInQueue>0 ){
        document.getElementById('server-status').innerText='busy';
        departedCustomers++;
        customersInQueue--;
        document.getElementById('queue').innerText=customersInQueue;
        beginService();         
    }else if(serverStat==='busy'&& customersInQueue===0 && currentCustomers===0 ){
        document.getElementById('server-status').innerText='idle';
    }else if(serverStat==='busy'&& customersInQueue>=0 && currentCustomers>0){
        currentCustomers--;
        document.getElementById('queue').innerText=customersInQueue;
        departedCustomers++;
        beginService();    
    }else if(serverStat==='busy'&& customersInQueue>=0 && currentCustomers>0){
        joinQueue(customersInQueue);
    }else if(serverStat==='idle'&& customersInQueue>0 && customersinSys>=0){
        document.getElementById('server-status').innerText='busy';
        customersInQueue--;
        departedCustomers++;
        beginService();      
    } 
    //set new departed customer value 
    document.getElementById('departed').innerText=departedCustomers;
    //set customers in system 
    document.getElementById('customers-in-system').innerText=currentCustomers;
    console.log('customer departed');
    if(currentCustomers===0){
        document.getElementById('server-icon').classList.remove('busy');
        document.getElementById('server-status').innerText='idle';
    }else{
        document.getElementById('server-icon').classList.add('busy');
        document.getElementById('server-status').innerText='busy';
    }
}

let totalNumCustomers=parseInt(document.getElementById('customers-count').innerText);
let customersinSys=parseInt(document.getElementById('customers-in-system').innerText);
//customer arrived (event)
function arrival(){    
    // customer total number -1 & customer in system +1 
    if(customersinSys>=0){
        document.getElementById('customers-count').innerText=--totalNumCustomers;
        document.getElementById('customers-in-system').innerText=customersinSys+1;
         //check if server is idle or busy 
    let serverStat= document.getElementById('server-status').innerText;
    if(serverStat.toLocaleLowerCase()=='idle' && customersInQueue===0){
        customersinSys+=customersinSys;
        document.getElementById('server-status').innerText='busy';
        document.getElementById('server-icon').classList.add('busy');
        beginService();
    }else if(serverStat.toLocaleLowerCase()=='busy' && customersInQueue>=0){
        joinQueue(customersInQueue);
    }
     }else{
         return 0;
     }
}
// show data in the table  
function addElemToTable(){
    customerNum++;
    let tbody=document.getElementById('table-body');
    //appendChild to Table
    let trow=document.createElement("tr")
    trow.classList.add('text-center'); 
    trow.innerHTML=`
                    <td>${customerNum}</td>
                    <td>${interarrivalTime/1000}</td>
                    <td>${arrivalOnClock[customerNum-1]}</td>
                    <td>${serviceTime/1000}</td>
                    <td>${timeServiceBegins[customerNum-1]}</td>
                    <td>${timeCustomerWait[customerNum-1]}</td>
                    <td>${endTime[customerNum-1]}</td>
                    <td>${timeCustomerSpends[customerNum-1]}</td>
                    <td>${idleTime[customerNum-1]}</td>`;
                    tbody.appendChild(trow);
                    return true;
}




