---
title: Examples
layout: default
parent: Code and Data download
nav_order: 3
---

# Examples

Here are some examples of how to use the data and code provided in this repository, 
also highlighting how to easily adjust the code to alter the workflow.

## Example 1: Volcano Plots

In this example, we will create a volcano plot using the example dataset. To recreate 
this example **within** cOmicsArt, use the following steps:

1. Start the Application (locally or [online](https://shiny.iaas.uni-bonn.de/cOmicsArt/))
2. In the `Data Selection`, use the `Testdata`
3. We want to use all the data, so we will not filter the data, directly clicking `"Start 
   the Journey"`
4. Select `DESeq2` as the pre-processing method with `condition` as the main factor
5. In the `Significance Analysis`, run the significance analysis for `trt:untrt`, Significance 
   level: `0.05` and test-correction: `Benjamini-Hochberg`
6. Select now the `trt:untrt` tab, in the the `Volcano` tab
7. Download the data and code by clicking on `Get underlying R code and data` under 
   `Volcanot plot padj`

Anything not mentioned here can be left as default. Below you can find a slide show of 
the steps to follow:

<!-- Container for the slideshow -->
<div style="display: flex; align-items: center; justify-content: center;">
  <!-- Left arrow -->
  <span id="prev" style="font-size: 2em; cursor: pointer; margin-right: 10px;">&#8592;</span>

  <!-- Image element -->
  <img id="slideshow" src="/cOmicsArt/assets/images/" width="600px">

  <!-- Right arrow -->
  <span id="next" style="font-size: 2em; cursor: pointer; margin-left: 10px;">&#8594;</span>
</div>

<!-- Subtitle element -->
<p id="subtitle" style="text-align: center;">Test 1</p>

<script>
var images = [
    {src: "/cOmicsArt/assets/images/cOmicsCat.png", subtitle: "Test 1"},
    {src: "/cOmicsArt/assets/images/cOmicsGiraffe.png", subtitle: "Test2"}
];
var currentIndex = 0;

function updateSlideshow(index) {
    document.getElementById("slideshow").src = images[index].src;
    document.getElementById("subtitle").textContent = images[index].subtitle;
}

document.getElementById("prev").onclick = function() {
    currentIndex = (currentIndex - 1 + images.length) % images.length;
    updateSlideshow(currentIndex);
};

document.getElementById("next").onclick = function() {
    currentIndex = (currentIndex + 1) % images.length;
    updateSlideshow(currentIndex);
};
</script>
