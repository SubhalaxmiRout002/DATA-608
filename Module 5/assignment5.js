//Beginner Problem 1
var inputTextEle, outputTextEle;
inputTextEle = document.getElementById("inputTextEle");
outputTextEle = document.getElementById("outputTextEle");


function reverseInputText() {
  if(inputTextEle && outputTextEle){
    outputTextEle.innerHTML = inputTextEle.value.split("").reverse().join("");
  }
}

//Beginner Problem 2
var inputNumberEle, inputNumberEleValue, multiplesTableContainer, tableEle, trEle;
inputNumberEle = document.getElementById("inputNumberEle");
multiplesTableContainer = document.getElementById("multiplesTableContainer");


function createMultiplesTable() {
  tableEle = document.createElement("table");
  trEle = document.createElement("tr");
  if(inputNumberEle && multiplesTableContainer){
    inputNumberEleValue = inputNumberEle.value;
    for(var i=1; i<21; i++){      
      var tdEle = document.createElement("td");
      var tdValueEle = document.createTextNode(inputNumberEleValue * i);
      tdEle.appendChild(tdValueEle);
      trEle.appendChild(tdEle);
      if(i == 4 || i == 8 || i == 12 || i == 16 || i == 20){
        tableEle.appendChild(trEle);
        trEle = document.createElement("tr");
      }  
    }
    if(multiplesTableContainer.firstChild){
      multiplesTableContainer.replaceChild(tableEle, multiplesTableContainer.firstChild);
    }
  }
}

//Intermediate problem 1
var inputPresidentNameEle, inputPresidentNameEleValue, heightEle, weightEle, presidentsDataTable, trEle, csvValue = [], findPresident = function(){};

presidentsDataTable = document.getElementById("presidentsDataTable");
heightEle = document.getElementById("heightEle");
weightEle = document.getElementById("weightEle");
trEle = document.createElement("tr");

d3.csv("https://raw.githubusercontent.com/SubhalaxmiRout002/DATA-608/main/Module%205/presidents.csv", function(data) {  
  csvValue.push(data);  
  trEle.innerHTML = '<td>' + data.Name + '</td>' + '<td>' + data.Height + '</td>' + '<td>' + data.Weight + '</td>';
  presidentsDataTable.appendChild(trEle);
  trEle = document.createElement("tr");
});

function showHeightAndWeight() {
  
  inputPresidentNameEle = document.getElementById("inputPresidentNameEle");
  inputPresidentNameEleValue = inputPresidentNameEle.value.toLowerCase();
  console.log(inputPresidentNameEleValue);
  let presidentObj = csvValue.find(findPresident => findPresident.Name.toLowerCase() == inputPresidentNameEleValue);
  if(presidentObj){
    heightEle.innerHTML = presidentObj.Height;
    weightEle.innerHTML = presidentObj.Weight;
  } else {
    heightEle.innerHTML = "Not Available";
    weightEle.innerHTML = "Not Available";
  }
}





