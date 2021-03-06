// set the dimensions and margins of the graph
var log = console.log;
var mainColor =  'rgb(200,53,71)';

var multiplesCont = $("#multiples")[0].getBoundingClientRect();
// $("main").css("width", multiplesCont.width);

var margin = {top: 30, right: 30, bottom: 50, left: 50},
    width = multiplesCont.width - margin.left * 2 - margin.right,
    height = 180 - margin.top - margin.bottom;


var meanSalary_2018 =  8867;
var xDomain = 100000;
var commonYAxis = true;
var xAxisMax = [];
var yAxisMax = [];



var line = d3.line()
    .curve(d3.curveBasis);


    // .call(d3.drag()
    //         .container(function() { return this; })
    //         .subject(function() { var p = [d3.event.x, d3.event.y]; return [p, p]; })
    //         .on("start", dragstarted))



var selected;
var desiredSalary = 0;
var desiredSphere = "Освіта";

var actualBinsData;


d3.csv("data/salaries.csv", function(fullData) {

    fullData.forEach(function(d) {
        d.mean = +d.mean/12;
        d.value = +d.value/12;
    });

    filterProffesions(fullData, desiredSalary, desiredSphere);
    addMultipleCharts();
    

    d3.select("#submit").on("click", function() {
        desiredSalary = $('input[type=number][name=getSalary]').val();
        console.log(desiredSalary);
        desiredSalary = +desiredSalary;
        filterProffesions(fullData, desiredSalary, desiredSphere)
    });


    $('select#sphere').on("change", function(){
        if(this.options[this.selectedIndex].value != ""){
            desiredSphere = this.options[this.selectedIndex].value;
        } else {
            desiredSphere = false
        }

        filterProffesions(fullData, desiredSalary, desiredSphere)
    });

    

});


function filterProffesions(given, desiredSalary, desiredSphere) {
    d3.select("#types ul").remove();

    var data;

    if (desiredSphere){
        data = given.filter(function(d) {
            return d.sphere === desiredSphere && d.mean > desiredSalary
        });
    } else {
        data = given.filter(function(d) {
           return d.mean > desiredSalary
        });
    }

    var classesTypeList = [];
    var classes = [];

    
    data.forEach(function(d){
        if(!classesTypeList.includes(d.type)){
            classesTypeList.push(d.type);
            classes.push(d)
        }
    });

    
    classes.sort(function(a,b){
        return d3.ascending(a.type, b.type)
    });
    

    var row = d3.select("#types")
        .append("ul")
        .attr("id", "myUL");

    
    classes.forEach(function(d){
        var myLI = row.append("li");  
        
        myLI.append("p")
            .html(d.type);
        
        myLI.append("img")
            .attr("src", "img/plus.png")
            .attr("class", "plus")
            .style("cursor", "pointer")
            .on("click", function(){
                selected = $(this).closest("li").text();
                var  newData = data.filter(function(k){
                    return k.type === selected;
                });
                if(commonYAxis === true){
                    addChartCommonScale(newData, selected)
                } else {
                    addChartPersonalScale(newData, selected)
                }
            });
    });
}



function addChartCommonScale(myData, key) {

    /* доступ до останнього svg */
    d3.selectAll(".remove").classed("current", false);

    /* додаємо і видаляємо svg по кліку */
    var svg = d3.select("#multiples")
        .insert("svg", ".remove")
        .attr("width", width + margin.left + margin.right)
        .attr("height", height + margin.top + margin.bottom)
        .attr("class", "current remove")
        .on("click", function(d) {
            var valueToRemove = $(this).closest("svg").attr("data");
            valueToRemove = +valueToRemove;
            log(yAxisMax);
            removeElement(yAxisMax, valueToRemove);
            log(yAxisMax);
            if(commonYAxis === true){
                update();
            } else {
                // updateToPersonal();
            }

            $(this).closest("svg").remove();
        })
         .append("g")
          .attr("transform", "translate(" + margin.left + "," + margin.top + ")");


    /* додаємо максимальне значення цих даних до попередніх */
    xAxisMax.push(d3.max(myData, function(d) { return +d.value;} ));

    /* визначаємо максимальне значення x з усіх наявних на екрані*/
    var xMax = d3.max(xAxisMax, function(d) { return d });

    var newList = [];
    myData.forEach(function(d){
        if(!newList.includes(d.type)){
            newList.push(d.type)
        }
    });

    var x = d3.scaleLinear()
        .domain([0, xDomain])
        .range([0, width]);

    /* перемальовуємо x-Axis для всіх графіків в залежності від значень найбільшого*/
    d3.selectAll(".x-axis")
        .transition()
        .duration(750)
        .call(d3.axisBottom(x).ticks(5).tickFormat(d3.format(".0s")));

    /* додаємо x-Axis графік в поточний */
    svg.append("g")
        .attr("class", "x-axis")
        .attr("transform", "translate(0," + height + ")")
        .call(d3.axisBottom(x).ticks(5).tickFormat(d3.format(".0s")));


    /* створюємо histogram*/
    const thresholds = d3.range(0, xDomain, (xDomain - 0) / 30);
    var histogram = d3.histogram()
        .value(function(d) { return d.value; })   // I need to give the vector of value
        .domain(x.domain())  // then the domain of the graphic
        .thresholds(thresholds); // then the numbers of bins

    var bins = histogram(myData);

    /* додаємо максимальне y-Axis значення цих даних до попередніх */
    yAxisMax.push(d3.max(bins, function(d) { return d.length; }));

    /* визначаємо актуальне максимальне */
    var yMax = d3.max(yAxisMax, function(d) { return d });

    var y = d3.scaleLinear()
        .range([height, 0])
        .domain([0, yMax]);

    d3.selectAll(".y-axis")
        .transition()
        .duration(750)
        .call(d3.axisLeft(y).ticks(5));

    svg.append("g")
        .attr("class", "y-axis")
        .call(d3.axisLeft(y).ticks(5));

    d3.selectAll(".bins")
        .transition()
        .duration(750)
        .attr("transform", function(d) { return "translate(" + x(d.x0) + "," + y(d.length) + ")"; })
        .attr("height", function(d) { return height - y(d.length); });

    /* додатковий атрибут svg, щоб отримати доступ до y-max до кожного графіка */
    var currentMax = d3.max(bins, function(d) { return d.length; });
    d3.select(".current").attr("data", currentMax);

    /* додаємо гістограму у поточний графік */
    svg.selectAll("rect")
        .data(bins)
        .enter()
        .append("rect")
        .attr("class", "bins")
        .attr("x", 0)
        .attr("transform", function(d) { return "translate(" + x(d.x0) + "," + y(d.length) + ")"; })
        .attr("width", function(d) { return x(d.x1) - x(d.x0) -1 ; })
        .attr("height", function(d) { return height - y(d.length); })
        .style("fill", mainColor);


    svg.append("line")
        .attr("class", "country-mean")
        .attr("x1", x(8867))
        .attr("x2", x(8867))
        .attr("y1", 0)
        .attr("y2", height)
        .style("stroke", "grey")
        .style("stroke-dasharray", "2");

    svg.append("line")
        .attr("class", "prof-mean")
        .attr("data", myData[0].mean)
        .attr("x1", x(myData[0].mean) )
        .attr("x2", x(myData[0].mean) )
        .attr("y1", 0)
        .attr("y2", height)
        .style("stroke-width", "1px")
        .style("stroke", "#00dfff");

    /* додаємо підпис */
    svg.append("text")
        .attr("text-anchor", "start")
        .attr("class", "multipleChartHeader")
        .attr("y", -5)
        .attr("x", 0)
        .text(key)
        .style("fill", mainColor);
}


//Якщо різна шкала
function addChartPersonalScale(myData, key) {
    /* доступ до останнього svg */
    d3.selectAll(".remove").classed("current", false);

    /* додаємо і видаляємо svg по кліку */
    var svg = d3.select("#multiples")
        .insert("svg", ".remove")
        .attr("width", width + margin.left + margin.right)
        .attr("height", height + margin.top + margin.bottom)
        .attr("class", "current remove")
        .on("click", function(d) {
            var valueToRemove = $(this).closest("svg").attr("data");
            valueToRemove = +valueToRemove;
            log(yAxisMax);
            removeElement(yAxisMax, valueToRemove);

            log(yAxisMax);
            if(commonYAxis === true){
                update();
            } else {
                // updateToPersonal();
            }
            $(this).closest("svg").remove();
        })
        .append("g")
        .attr("transform", "translate(" + margin.left + "," + margin.top + ")");


    /* додаємо максимальне значення цих даних до попередніх */
    xAxisMax.push(d3.max(myData, function(d) { return +d.value;} ));

    /* визначаємо максимальне значення x з усіх наявних на екрані*/
    var xMax = d3.max(xAxisMax, function(d) { return d });

    var newList = [];
    myData.forEach(function(d){
        if(!newList.includes(d.type)){
            newList.push(d.type)
        }
    });

    var x = d3.scaleLinear()
        .domain([0, xDomain])
        .range([0, width]);

    /* перемальовуємо x-Axis для всіх графіків в залежності від значень найбільшого*/
    d3.selectAll(".x-axis")
        .transition()
        .duration(750)
        .call(d3.axisBottom(x).ticks(5).tickFormat(d3.format(".0s")));

    /* додаємо x-Axis графік в поточний */
    svg.append("g")
        .attr("class", "x-axis")
        .attr("transform", "translate(0," + height + ")")
        .call(d3.axisBottom(x).ticks(5).tickFormat(d3.format(".0s")));


    /* створюємо histogram*/
    const thresholds = d3.range(0, xDomain, (xDomain - 0) / 30);
    var histogram = d3.histogram()
        .value(function(d) { return d.value; })   // I need to give the vector of value
        .domain(x.domain())  // then the domain of the graphic
        .thresholds(thresholds); // then the numbers of bins

    var bins = histogram(myData);
    var yDomain = d3.max(bins, function(d) { return d.length; });
    /* додаємо максимальне y-Axis значення цих даних до попередніх */
    yAxisMax.push(d3.max(bins, function(d) { return d.length; }));

    /* визначаємо актуальне максимальне */
    var yMax = d3.max(yAxisMax, function(d) { return d });

    var y = d3.scaleLinear()
        .range([height, 0])
        .domain([0, yDomain]);

    svg.append("g")
        .attr("class", "y-axis")
        .call(d3.axisLeft(y).ticks(5));

    /* додатковий атрибут svg, щоб отримати доступ до y-max до кожного графіка */
    var currentMax = d3.max(bins, function(d) { return d.length; });
    d3.select(".current").attr("data", currentMax);



    /* додаємо гістограму у поточний графік */
    svg.selectAll("rect")
        .data(bins)
        .enter()
        .append("rect")
        .attr("class", "bins")
        .attr("x", 0)
        .attr("transform", function(d) { return "translate(" + x(d.x0) + "," + y(d.length) + ")"; })
        .attr("width", function(d) { return x(d.x1) - x(d.x0) -1 ; })
        .attr("height", function(d) { return height - y(d.length); })
        .style("fill", mainColor);


    svg.append("line")
        .attr("class", "country-mean")
        .attr("x1", x(8867))
        .attr("x2", x(8867))
        .attr("y1", 0)
        .attr("y2", height)
        .style("stroke", "grey")
        .style("stroke-dasharray", "2");

    svg.append("line")
        .attr("class", "prof-mean")
        .attr("data", myData[0].mean)
        .attr("x1", x(myData[0].mean) )
        .attr("x2", x(myData[0].mean) )
        .attr("y1", 0)
        .attr("y2", height)
        .style("stroke", "#005AD3");

    /* додаємо підпис */
    svg.append("text")
        .attr("text-anchor", "start")
        .attr("y", -5)
        .attr("x", 0)
        .text(key)
        .style("fill", mainColor);
}




function update() {
    var yMax = d3.max(yAxisMax, function(d) { return d });
    var xMax = d3.max(xAxisMax, function(d) { return d });

    var x = d3.scaleLinear()
        .domain([0, xDomain])
        .range([0, width]);

    var y = d3.scaleLinear()
        .range([height, 0])
        .domain([0, yMax]);

    d3.selectAll(".y-axis")
        .transition()
        .duration(750)
        .call(d3.axisLeft(y).ticks(5));

    d3.selectAll(".bins")
        .transition()
        .duration(750)
        .attr("transform", function(d) { return "translate(" + x(d.x0) + "," + y(d.length) + ")"; })
        // .attr("width", function(d) { return x(d.x1) - x(d.x0) -1 ; })
        .attr("height", function(d) { return height - y(d.length); });
}


$(window).on("resize",  function(d){
    var newMultiplesCont = $("#multiples")[0].getBoundingClientRect();
    var newWidth = newMultiplesCont.width - margin.left * 2 - margin.right;

    width = newWidth;


    var yMax = d3.max(yAxisMax, function(d) { return d });
    var xMax = d3.max(xAxisMax, function(d) { return d });

    var newX = d3.scaleLinear()
        .domain([0, xDomain])
        .range([0, newWidth]);

    var newY = d3.scaleLinear()
        .range([height, 0])
        .domain([0, yMax]);

    d3.selectAll("svg")
        .attr("width", newWidth + margin.left + margin.right);

    d3.selectAll(".x-axis")
        .transition()
        .duration(750)
        .call(d3.axisBottom(newX).ticks(5).tickFormat(d3.format(".0s")));

    d3.selectAll(".bins")
        .transition()
        .duration(750)
        .attr("transform", function(d) { return "translate(" + newX(d.x0) + "," + newY(d.length) + ")"; })
        .attr("width", function(d) {
            return newX(d.x1) - newX(d.x0) - 1 ;
        });

    d3.selectAll("line.country-mean")
        .transition()
        .duration(750)
        .attr("x1", newX(8867))
        .attr("x2", newX(8867));

    d3.selectAll("line.prof-mean")
        .transition()
        .duration(750)
        .attr("x1", function() {
            var myValue = $(this).attr("data");
            myValue = + myValue;
            return newX(myValue)
        })
        .attr("x2", function() {
            var myValue = $(this).attr("data");
            myValue = + myValue;
            return newX(myValue)
        })

});





function updateToPersonal() {
   $("svg").each(function(i){
        var currentChart = $("svg")[i];
        var currentChartDomain = $(currentChart).attr("data");
        console.log(currentChartDomain);

        var x = d3.scaleLinear()
            .domain([0, xDomain])
            .range([0, width]);

        var y = d3.scaleLinear()
            .range([height, 0])
            .domain([0, currentChartDomain]);

        var currentY = $(currentChart.childNodes[0]).find(".y-axis")[0];
        
        d3.select(currentY)
            .transition()
            .duration(750)
            .call(d3.axisLeft(y).ticks(5));
       
        var currentBins = $(currentChart.childNodes[0]).find("rect");
        
        d3.selectAll(currentBins)
            .transition()
            .duration(750)
            .attr("transform", function(d) { return "translate(" + x(d.x0) + "," + y(d.length) + ")"; })
            .attr("height", function(d) { return height - y(d.length); });

    });

}



function addMultipleCharts() {

    var target = $("#types ul li");

    for(var i = 0; i < target.length; i++){
        $(".plus")[i].click()
    }
}



//Міняємо шкалу активна/неактивна
d3.select("#yAxisType button").on("click", function() {
    commonYAxis = !commonYAxis;
    if(commonYAxis === true) {
        d3.select("#yAxisType button").text("Єдина Y-шкала (акт.)");
        update()
    } else {
        d3.select("#yAxisType button").text("Єдина Y-шкала (неакт.)");
        updateToPersonal()
    }
});


//видалити значення Y-шкали, після видалення графіка
function removeElement(array, elem) {
    var index = array.indexOf(elem);
    if (index > -1) {
        array.splice(index, 1);
    }
}

//пошук професії
function myFunction() {
    // Declare variables
    var input, filter, ul, li, p, i, txtValue;
    input = document.getElementById('myInput');
    filter = input.value.toUpperCase();
    ul = document.getElementById("myUL");
    li = ul.getElementsByTagName('li');

    // Loop through all list items, and hide those who don't match the search query
    for (i = 0; i < li.length; i++) {
        p = li[i].getElementsByTagName("p")[0];
        txtValue = p.textContent || p.innerText;
        if (txtValue.toUpperCase().indexOf(filter) > -1) {
            li[i].style.display = "";
        } else {
            li[i].style.display = "none";
        }
    }
}








// // Compute kernel density estimation
// var kde = kernelDensityEstimator(kernelEpanechnikov(7), x.ticks(30));
//
// // d3.select(".x-axis")
// //     .data(myData)
// //     .transition()
// //     .duration(750)
// //     .call(d3.axisBottom(x));
//
//
// newList.forEach(function(item){
//     var density1 =  kde( myData
//         .filter( function(d){return d.type === item} )
//         .map(function(d){  return d.value; }) );
//
//
//     // Plot the area
//     svg.append("path")
//         .datum(density1)
//         .attr("class", "mypath")
//         .attr("fill", "#69b3a2")
//         .attr("opacity", "0.3")
//         .attr("stroke", "#000")
//         .attr("stroke-width", 1)
//         .attr("stroke-linejoin", "round")
//         .attr("d",  d3.line()
//             .curve(d3.curveBasis)
//             .x(function(d) { return x(d[0]); })
//             .y(function(d) { return y(d[1]); })
//         );
//
// });


// Function to compute density
// function kernelDensityEstimator(kernel, X) {
//     return function(V) {
//         return X.map(function(x) {
//             return [x, d3.mean(V, function(v) { return kernel(x - v); })];
//         });
//     };
// }
// function kernelEpanechnikov(k) {
//     return function(v) {
//         return Math.abs(v /= k) <= 1 ? 0.75 * (1 - v * v) / k : 0;
//     };
// }
//
//
// function dragstarted() {
//     var d = d3.event.subject,
//         active = svg.append("path").attr("class", "draggedPath").datum(d),
//         x0 = d3.event.x,
//         y0 = d3.event.y;
//
//     d3.event.on("drag", function() {
//         var x1 = d3.event.x,
//             y1 = d3.event.y,
//             dx = x1 - x0,
//             dy = y1 - y0;
//
//         if (dx * dx + dy * dy > 100) d.push([x0 = x1, y0 = y1]);
//         else d[d.length - 1] = [x1, y1];
//         active.attr("d", line);
//     });
//
//     d3.event.on("end", function() {
//         alert("finished");
//         d3.selectAll(".draggedPath").remove()
//     });
// }
