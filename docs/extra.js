'use strict';
window.onload = init;

function init() {

const mapElement = document.getElementById("mapid");

var box = [-117.06278, 45.22677, -114.29213, 48.99998];

// define rectangle geographical bounds
var bounds = [[box[1], box[0]], [box[3], box[2]]];


const OpenStreetMap_Mapnik = L.tileLayer('https://tile.openstreetmap.org/{z}/{x}/{y}.png', {
	maxZoom: 19,
	attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
});


const Esri_WorldImagery = L.tileLayer('https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}', {
	attribution: 'Tiles &copy; Esri &mdash; Source: Esri, i-cubed, USDA, USGS, AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP, and the GIS User Community'
});

var map = L.map(mapElement, {
layers: [Esri_WorldImagery]
}).fitBounds(bounds);


const baseLayers = {
    'EsriWorldImagery': Esri_WorldImagery,
    'OpenStreetMap': OpenStreetMap_Mapnik
  }

var streamStyle = function(feature, layer){
switch(feature.properties.final_score){

  case 1: return layer.setStyle({fillColor: "#0D0887FF"});
  case 2: return layer.setStyle({fillColor: "#47039FFF"});
  case 3: return layer.setStyle({fillColor: "#7301A8FF"});
  case 4: return layer.setStyle({fillColor: "#9C179EFF"});
  case 5: return layer.setStyle({fillColor: "#BD3786FF"});
  case 6: return layer.setStyle({fillColor:  "#D8576BFF"});
  case 7: return layer.setStyle({fillColor: "#ED7953FF"});
  case 8: return layer.setStyle({fillColor:  "#FA9E3BFF" });
  case 9: return layer.setStyle({fillColor: "#FDC926FF"});
  case 10: return layer.setStyle({fillColor: "#F0F921FF"});

}

}

var lakeStyle = function(feature, layer){

  switch(feature.properties.final_score){

  case 1: return layer.setStyle({fillColor: "#0D0887FF"});
  case 2: return layer.setStyle({fillColor: "#47039FFF"});
  case 3: return layer.setStyle({fillColor: "#7301A8FF"});
  case 4: return layer.setStyle({fillColor: "#9C179EFF"});
  case 5: return layer.setStyle({fillColor: "#BD3786FF"});
  case 6: return layer.setStyle({fillColor:  "#D8576BFF"});
  case 7: return layer.setStyle({fillColor: "#ED7953FF"});
  case 8: return layer.setStyle({fillColor:  "#FA9E3BFF" });
  case 9: return layer.setStyle({fillColor: "#FDC926FF"});
  case 10: return layer.setStyle({fillColor: "#F0F921FF"});
}

}


function getColor(d) {
 switch(d){

  case 1: return "#0D0887FF"
  case 2: return "#47039FFF"
  case 3: return "#7301A8FF"
  case 4: return "#9C179EFF"
  case 5: return "#BD3786FF"
  case 6: return  "#D8576BFF"
  case 7: return "#ED7953FF"
  case 8: return  "#FA9E3BFF"
  case 9: return "#FDC926FF"
  case 10: return "#F0F921FF"
  case null: return "#6c9ad5"
}
}


var popupFunc = function(feature) {

  return  '<strong><center><div style="font-size:14px;">' + 'Summary Table' + '</div></center></strong>' +
          '</strong><div style="font-size:11px;"><b>Final Score: </b>' + feature.properties.final_score +
          '<div class="box" style="background-color:' + getColor(feature.properties.final_score) + '"></div> ' +
          '</strong><br><b>Final Score Social: </b>' + feature.properties.final_score_social +
          '</strong><br><b>Final Score Habitat: </b>' + feature.properties.final_score_habitat +
          '</strong><br><b>HUC 12: </b>' + feature.properties.huc12 +
          '</strong><br><b>comid: </b>' + feature.properties.comid + '</div>'

}


/// customTip function

function customTip(layer,feature) {
    layer.unbindTooltip();
    if(!layer.isPopupOpen()) layer.bindTooltip(popupFunc(feature), {className: 'myCSSClass'}).openTooltip();
}

/// Adding the final model layers

$.getJSON("final_streams.geojson", function(data) {

  let streams = L.geoJSON(data,{
  style:
    {'color': '#000',
    'fillOpacity': 0.8,
    'opacity': 0.5,
    'weight':1},
  onEachFeature: function onEachFeature(feature, layer) {
                //layer.bindPopup(popupFunc(feature), {className: 'myCSSClass'}).openPopup();

layer.on('mouseover', customTip(layer,feature));

streamStyle(feature, layer);

}
  }).addTo(map);

layerControls.addOverlay(streams, 'Final Score (streams)')


});

$.getJSON("final_lakes.geojson", function(data) {

let wb =  L.geoJSON(data, {

  style:
    {'color': '#000',
    'fillOpacity': 0.8,
    'opacity': 0.5,
    'weight':1},
  onEachFeature: function onEachFeature(feature, layer) {
                //layer.bindPopup(popupFunc(feature), {className: 'myCSSClass'}).openPopup();

layer.on('mouseover', customTip(layer,feature));

lakeStyle(feature, layer);

  }

  }).addTo(map);


layerControls.addOverlay(wb, 'Final Score (waterbodies)')

});

var monitorStyle = {
  radius: 50,
  color: 'black',
  weight: 0.25,
  fillOpacity: 1,
  fillColor: 'red'
}
// cluster

var markers_ais = L.markerClusterGroup({
	spiderfyOnMaxZoom: true,
	showCoverageOnHover: true,
	zoomToBoundsOnClick: true
});

var markers_pibo = L.markerClusterGroup({
	spiderfyOnMaxZoom: true,
	showCoverageOnHover: true,
	zoomToBoundsOnClick: true
});

var markers_local = L.markerClusterGroup({
	spiderfyOnMaxZoom: true,
	showCoverageOnHover: true,
	zoomToBoundsOnClick: true
});

/// customTip function

var popupFuncPibo = function(feature) {

  return  '<strong><center><div style="font-size:14px;">' + 'PIBO Site' + '</div></center></strong>' +
          '</strong><div style="font-size:11px;"><b>Stream: </b>' + feature.properties.Stream +
          '</strong><br><b>Forest: </b>' + feature.properties.Forest +
          '</strong><br><b>District: </b>' + feature.properties.District +
          '</strong><br><b>Type: </b>' + feature.properties.Mgmt + '</div>'

}

var popupFuncLocal = function(feature) {

  return  '<strong><center><div style="font-size:14px;">' + 'PIBO Site' + '</div></center></strong>' +
          '</strong><div style="font-size:11px;"><b>Site: </b>' + feature.properties.Site_Name +'</div>'

}


function customTipPibo(layer,feature) {
    layer.unbindTooltip();
    if(!layer.isPopupOpen()) layer.bindTooltip(popupFuncPibo(feature), {className: 'myCSSClass'}).openTooltip();
}


function customTipLocal(layer,feature) {
    layer.unbindTooltip();
    if(!layer.isPopupOpen()) layer.bindTooltip(popupFuncLocal(feature), {className: 'myCSSClass'}).openTooltip();
}


$.getJSON("monitoring.geojson", function(data) {

 let monitoring = L.geoJSON(data);

markers_ais.addLayer(monitoring);

layerControls.addOverlay(markers_ais, 'AIS Monitoring Sites')

});


$.getJSON("pibo.geojson", function(data) {

 let pibo = L.geoJSON(data,{
                onEachFeature: function onEachFeature(feature, layer) {
                //layer.bindPopup(popupFuncPibo(feature), {className: 'myCSSClass'}).openPopup();

                layer.on('mouseover', customTipPibo(layer,feature));

                }

 });

markers_pibo.addLayer(pibo);

layerControls.addOverlay(markers_pibo, 'PIBO Monitoring Sites')

});


$.getJSON("local.geojson", function(data) {

 let local = L.geoJSON(data,{
                onEachFeature: function onEachFeature(feature, layer) {
                //layer.bindPopup(popupFuncPibo(feature), {className: 'myCSSClass'}).openPopup();

                layer.on('mouseover', customTipLocal(layer,feature));

                }

 });

markers_local.addLayer(local);

layerControls.addOverlay(markers_local, 'Local Monitoring Sites')

});


var legend = L.control({position: 'bottomright'});

legend.onAdd = function (map) {

    var div = L.DomUtil.create('div', 'info legend'),
        grades = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, null],
        labels = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 'NA'];

    div.innerHTML += '<strong> Final Scores </strong><br>'

    // loop through our density intervals and generate a label with a colored square for each interval
    for (var i = 0; i < grades.length; i++) {
        div.innerHTML +=
            '<i style="background:' + getColor(grades[i]) + '"></i> '+
            labels[i] + '<br>';
    }

    return div;
};

legend.addTo(map);


const layerControls = L.control.layers(baseLayers, {}, {}).addTo(map);

// adding controls
L.Control.Watermark = L.Control.extend({
    onAdd: function(map) {
        var img = L.DomUtil.create('img');

        img.src = 'https://www.fs.usda.gov/themes/custom/fs_uswds/logo.svg';
        img.style.width = '50px';

        return img;
    },

    onRemove: function(map) {
        // Nothing to do here
    }
});

L.control.watermark = function(opts) {
    return new L.Control.Watermark(opts);
}

L.control.watermark({ position: 'bottomleft' }).addTo(map);


L.Control.MyControl = L.Control.extend({
  options: {
    position: 'topright'
  },
  onAdd: function(map) {
    var button = L.DomUtil.create('button', 'open-modal-btn');
    button.innerText='Scoring Matrix';
    button.onclick = function() {
      var myModal = new bootstrap.Modal(document.getElementById('myModal'));
      myModal.show();
    }


    return button;

  },

  onRemove: function(map) {
    // Nothing to do here
  }
});

map.addControl(new L.Control.MyControl());

var downloadControl = L.Control.extend({
    options: {
        position: 'topright' // You can change the position based on your needs
    },

    onAdd: function (map) {
        var button = L.DomUtil.create('button', 'open-modal-btn');
    button.innerText='Download Data';

        button.onclick = function(){
            var myModal = new bootstrap.Modal(document.getElementById('downloadModal'));
            myModal.show();
        }

        return button;
    },

  onRemove: function(map) {
    // Nothing to do here
  }

});

map.addControl(new downloadControl());

var scoringMatrix = document.getElementById('scoringMatrix');

scoringMatrix.onclick = function() {
      var myModal = new bootstrap.Modal(document.getElementById('myModal'));
      myModal.show();
    }

var finalScores = document.getElementById('finalScores');

finalScores.onclick = function() {
      var myScoreModal = new bootstrap.Modal(document.getElementById('finalScoresModal'));
      myScoreModal.show();
    }

}




