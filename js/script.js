// set the dimensions and margins of the graph
var log = console.log;
var mainColor =  '#B8001D';
var margin = {top: 30, right: 30, bottom: 50, left: 50},
    width = 300 - margin.left - margin.right,
    height = 250 - margin.top - margin.bottom;


var line = d3.line()
    .curve(d3.curveBasis);


    // .call(d3.drag()
    //         .container(function() { return this; })
    //         .subject(function() { var p = [d3.event.x, d3.event.y]; return [p, p]; })
    //         .on("start", dragstarted))



var classes = [];
var selected;


d3.csv("data/salaries.csv", function(data) {
    data.forEach(function(d){
        d.value = +d.value/12;
        if(!classes.includes(d.type)){
            classes.push(d.type)
        }
    });

    classes.sort(function(a,b){
        return d3.ascending(a, b)
    });

    var row = d3.select("#types")
        .append("ul")
        .attr("id", "myUL");


    classes.forEach(function(d){

        var myLI = row.append("li")
            .on("click", function(){

            });


        myLI.append("p")
            .html(d);


        myLI.append("img")
            .attr("src", "img/plus.png")
            .attr("class", "plus")
            .style("cursor", "pointer")
            .on("click", function(){
                selected = $(this).closest("li").text();
                var  newData = data.filter(function(k){
                    return k.type === selected;
                });

                databind(newData, selected);

            });

    });
});



var xAxisMax = [];
var yAxisMax = [];


function databind(myData, key) {

    /* доступ до останнього svg */
    d3.selectAll(".remove").classed("current", false);

    /* додаємо і видаляємо svg по кліку */
    var svg = d3.select("#multiples")
        .append("svg")
        .attr("width", width + margin.left + margin.right)
        .attr("height", height + margin.top + margin.bottom)
        .attr("class", "current remove")
        .on("click", function(d) {
            var valueToRemove = $(this).closest("svg").attr("data");
            valueToRemove = +valueToRemove;
            log(yAxisMax);
            removeElement(yAxisMax, valueToRemove);

            // yAxisMax = _.without(yAxisMax, +valueToRemove);
            log(yAxisMax);
            update();
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
        .domain([0, 100000])
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

    const constX = 100000;
    /* створюємо histogram*/
    const thresholds = d3.range(0, constX, (constX - 0) / 40);
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
        // .attr("width", function(d) { return x(d.x1) - x(d.x0) -1 ; })
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
        .domain([0, 100000])
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
function kernelDensityEstimator(kernel, X) {
    return function(V) {
        return X.map(function(x) {
            return [x, d3.mean(V, function(v) { return kernel(x - v); })];
        });
    };
}
function kernelEpanechnikov(k) {
    return function(v) {
        return Math.abs(v /= k) <= 1 ? 0.75 * (1 - v * v) / k : 0;
    };
}


function dragstarted() {
    var d = d3.event.subject,
        active = svg.append("path").attr("class", "draggedPath").datum(d),
        x0 = d3.event.x,
        y0 = d3.event.y;

    d3.event.on("drag", function() {
        var x1 = d3.event.x,
            y1 = d3.event.y,
            dx = x1 - x0,
            dy = y1 - y0;

        if (dx * dx + dy * dy > 100) d.push([x0 = x1, y0 = y1]);
        else d[d.length - 1] = [x1, y1];
        active.attr("d", line);
    });

    d3.event.on("end", function() {
        alert("finished");
        d3.selectAll(".draggedPath").remove()
    });
}


function removeElement(array, elem) {
    var index = array.indexOf(elem);
    if (index > -1) {
        array.splice(index, 1);
    }
}


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