/**
 * Created by yevheniia on 05.06.19.
 */
d3.csv("data/agrep_mistakes_result.csv", function(mistakes) {

    mistakes.forEach(function(d) {
        return d.freq = + d.freq
    });

    var TRUEmistakes = mistakes.filter(function (d){
        return d.mistake === "TRUE" && d.categoryBig != "апостроф"
    });


    var nested_data = d3.nest()
        .key(function(d) { return d.category; })
        .sortValues(d3.descending)
        .entries(TRUEmistakes);



    var container = d3.select("#MisprintButtons");

    var oneMisprint = container.selectAll("div")
        .data(nested_data)
        .enter()
        .append("div");

    oneMisprint.append("button")
            .attr("class", "collapsible")
            .text(function(d) {
                return d.key
            })
            .on("click", function(ev){
                let parent = $(this).parent();
                parent.find(".content").toggleClass("hideElem");
            });

    var pCont = oneMisprint
        .append("div")
        .attr("class", "content hideElem");


    var jokeWidth = $("#joke")[0].getBoundingClientRect().width / 600;


    var pcontent = pCont.selectAll("p")
        .data(function(d) {
            return d.values
        })
        .enter()
        .append("p");

    pcontent
        .attr("class", "misprintCases")
        .style("position", "relative")
        .text(function(k){
                return k.case + " / " + k.freq
           });

    pcontent.append("span")
            .style('display', "block")
            .style("position", "absolute")
            .style("top", "0")
            .style("left", "0")
            .style("z-index", "-1")
            .style("height", "20px")
            .style("width", function(k){ return  k.freq * jokeWidth  + "px" })
            .style("background-color", "#F56380") ;




    pCont.selectAll(".misprintCases").sort(function(a,b){
        return d3.ascending(a.freq, b.freq)
    })

    });