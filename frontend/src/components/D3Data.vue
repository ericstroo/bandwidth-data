<script setup>
import * as d3 from 'd3';

const ispData =  d3.csv("data/logdata.csv", (d) => {
    console.log(d);
}
);
console.log(ispData)

</script>
<template>
    hiss
</template>