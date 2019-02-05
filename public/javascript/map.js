function makeMap() {
  var claimStyles = {
    claimed_by_you: {
      "fillColor": "#9d5fa7", "color": "#111111", "weight": 1, "opacity": 0.65, "fillOpacity": 0.8
    }, //Purple
    unclaimed: {
      "fillColor": "#ffc746", "color": "#111111", "weight": 1, "opacity": 0.65, "fillOpacity": 0.2
    }, //Yellow
    claimed: {
      "fillColor": "#d5545a", "color": "#111111", "weight": 1, "opacity": 0.65, "fillOpacity": 0.8
    }, //Red
    quarantine: {
      "fillColor": "#6dbd4b", "color": "#111111", "weight": 1, "opacity": 0.65, "fillOpacity": 0.8
    }, //Green
  };

  var map = L.map('map');

  var mesh_layer; //Rendered map
  var last_update_bounds;
  var last_update_centroid;

  var tileLayer = L.tileLayer('http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png', {
    attribution: '&copy; <a href="http://osm.org/copyright">OpenStreetMap</a> contributors'
  }).addTo(map);

  $('#address_search_form').submit(function (event) {
    event.preventDefault();
    FitPcode($('#address_search').val());
  });

  var FitPcode = function (pcode) {
    $.getJSON('/pcode_get_bounds?pcode=' + pcode, function (json) {
      if (json) {
        map.fitBounds([[json.swlat, json.swlng], [json.nelat, json.nelng]])
      }
      else { alert("Postcode not found"); }
    });
  };

  var FindLocation = function () {
    var lat = Cookies.get("lat");
    var lng = Cookies.get("lng");
    var pcode = Cookies.get("postcode");
    if (lat && lng) {
      var zoom = Cookies.get("zoom");
      if (zoom) {
        map.setView([lat, lng], zoom)
      }
      else {
        map.setView([lat, lng], 15)
      }
    }
    else if (pcode) {
      FitPcode(pcode)
    }
    else {
      var australia_coord = [-29.8650, 131.2094];
      map.setView(australia_coord, 5);
    }
  };

  FindLocation();

  function addGeoJsonProperties(json) {

    var layer = L.geoJson(json, {
      style: function (feature) {
        switch (feature.properties.claim_status) {
          case 'claimed_by_you': return claimStyles.claimed_by_you
          case 'claimed': return claimStyles.claimed
          case 'quarantine': return claimStyles.quarantine
          default: return claimStyles.unclaimed
        }
      },
      onEachFeature: function (feature, featureLayer) {
        featureLayer._leaflet_id = feature.properties.slug;

        function downloadmesh(mesh_id) {
          var campaign = $('#campaign').val();
          var url = LAMBDA_BASE_URL + '/map?slug=' + mesh_id + '&campaign=' + campaign;
          var base64str = $.get(url, function (base64str) {
            if (base64str.message == "Internal server error") {
              return alert("This area cannot be downloaded due to a pdf rendering error, please try another area.");
            };
            //decode base64 string
            var binary = atob(base64str.base64.replace(/\s/g, ''));
            var len = binary.length;
            var buffer = new ArrayBuffer(len);
            var view = new Uint8Array(buffer);
            for (var i = 0; i < len; i++) {
              view[i] = binary.charCodeAt(i);
            }

            //create the blob object
            var blob = new Blob([view], { type: "application/pdf" });

            //create clickable URL for download
            var url = window.URL.createObjectURL(blob);
            var a = document.createElement('a');
            a.href = url;
            a.download = mesh_id + '.pdf';
            a.click();
            $('#load').addClass('hidden');
          })
        };

        this.btnClaim = function (featureLayer) {
          var leaflet_id = this._leaflet_id;
          $.post("/claim_meshblock/" + leaflet_id);
          $('.unclaim').removeClass('hidden');
          $('.download').removeClass('hidden');
          $('.claim').addClass('hidden');
          if ($("#map").data("is-admin") === true) {
            this.setStyle(claimStyles.quarantine)
          } else {
            this.setStyle(claimStyles.claimed_by_you)
          }
          $('#load').removeClass('hidden');
          downloadmesh(leaflet_id);
        }

        this.btnUnclaim = function (featureLayer) {
          $.post("/unclaim_meshblock/" + this._leaflet_id);
          this.setStyle(claimStyles.unclaimed)
          $('.unclaim').addClass('hidden');
          $('.download').addClass('hidden');
          $('.claim').removeClass('hidden');
        }

        this.btnDownload = function (featureLayer) {
          var leaflet_id = this._leaflet_id;
          $('#load').removeClass('hidden');
          downloadmesh(leaflet_id);
        }

        function create_popup_btn(container, div_class, btn_text_inner, faq_text_inner) {
          var grpdiv = L.DomUtil.create('div', 'popupgrp hidden ' + div_class, container)
          var txtdiv = L.DomUtil.create('div', 'popuptxt txt' + div_class, grpdiv)
          txtdiv.innerHTML = faq_text_inner
          var btndiv = L.DomUtil.create('div', 'popupbutton btn' + div_class, grpdiv)
          var btn = L.DomUtil.create('button', '', btndiv)
          btn.setAttribute('type', 'button')
          btn.setAttribute('name', div_class)
          btn.innerHTML = btn_text_inner
          var btndom = L.DomEvent
          btndom.addListener(btn, 'click', L.DomEvent.stopPropagation)
          btndom.addListener(btn, 'click', L.DomEvent.preventDefault)
          return { grpdiv, btn, btndom }
        };

        var container = L.DomUtil.create('div')
        var claimout = create_popup_btn(container, 'claim', 'Claim + Download',
          'Click to claim area and download PDF of addresses to doorknock.<br>');
        claimout.btndom.addListener(claimout.btn, 'click', this.btnClaim, featureLayer);
        var unclaimout = create_popup_btn(container, 'unclaim', 'Unclaim',
          'Click to remove your claim on this area.<br>');
        unclaimout.btndom.addListener(unclaimout.btn, 'click', this.btnUnclaim, featureLayer);
        var downloadout = create_popup_btn(container, 'download', 'Download',
          'Click to download your claimed area.<br>')
        downloadout.btndom.addListener(downloadout.btn, 'click', this.btnDownload, featureLayer);
        var otherstxtcontainer = L.DomUtil.create('div', 'popuptxt hidden otherstext', container)
        otherstxtcontainer.innerHTML = 'This area is claimed by someone else and is unable to be claimed.'
        var quarantinetxtcontainer = L.DomUtil.create('div', 'popuptxt hidden quarantinetext', container)
        quarantinetxtcontainer.innerHTML = 'This area is coordinated by a central event. ' +
          '<a href="' + $("#map").data("central-events-url") + '" target="_blank">Click here</a> to find it.';
        if (feature.properties.claim_status === 'claimed_by_you') {
          L.DomUtil.removeClass(unclaimout.grpdiv, 'hidden');
          L.DomUtil.removeClass(downloadout.grpdiv, 'hidden');
          var popup = L.popup({}, featureLayer).setContent(container);
        }
        else if (feature.properties.claim_status === 'claimed') {
          L.DomUtil.removeClass(otherstxtcontainer, 'hidden');
          var popup = L.popup({}, featureLayer).setContent(container);
        }
        else if (feature.properties.claim_status === 'quarantine') {
          if ($("#map").data("is-admin") === true) {
            L.DomUtil.removeClass(unclaimout.grpdiv, 'hidden');
            L.DomUtil.removeClass(downloadout.grpdiv, 'hidden');
          } else {
            L.DomUtil.removeClass(quarantinetxtcontainer, 'hidden');
          }
          var popup = L.popup({}, featureLayer).setContent(container);
        }
        else {
          L.DomUtil.removeClass(claimout.grpdiv, 'hidden');
          var popup = L.popup({}, featureLayer).setContent(container);
        }
        featureLayer.bindPopup(popup)

      }
    });

    return layer;
  };

  function getMeshblockCallback(json) {
    last_update_bounds = map.getBounds()
    last_update_centroid = map.getCenter()
    if (mesh_layer) { map.removeLayer(mesh_layer) }
    mesh_layer = addGeoJsonProperties(json)
    mesh_layer.addTo(map)
    $('#load').addClass('hidden')
  };

  var instruct = L.control();
  instruct.onAdd = function (map) {
    this._div = L.DomUtil.create('div', 'instruct');
    this._div.innerHTML = "Zoom in further to load doorknockable areas.";
    this.update();
    return this._div;
  }

  instruct.update = function () {
    $(".instruct").toggleClass('hidden', map.getZoom() > 14)
  }

  instruct.addTo(map);

  function updateMap() {
    var distance_moved, reload_dist
    var lat_lng_bnd = map.getBounds();
    var lat_lng_centroid = map.getCenter();
    Cookies.set("lat", lat_lng_centroid.lat);
    Cookies.set("lng", lat_lng_centroid.lng);
    if (last_update_centroid) {
      distance_moved = lat_lng_centroid.distanceTo(last_update_centroid)
    } else {
      distance_moved = 201
    }
    var zoom = map.getZoom();
    if (zoom > 14) {
      reload_dist = 1000 / (zoom - 14);
    } else {
      reload_dist = 0;
    }
    Cookies.set("zoom", zoom);

    //Reload map if zoom not too high
    //Distance moved is not short
    //and
    //there is no last_update or the current map bounds are not within the last update's
    if (zoom > 14 && (!last_update_bounds || distance_moved > reload_dist) &&
      (!last_update_bounds || !last_update_bounds.contains(lat_lng_bnd))) {
      $('#load').removeClass('hidden');

      var data = {
        sey: lat_lng_bnd.getSouthWest().lat,
        sex: lat_lng_bnd.getSouthWest().lng,
        nwy: lat_lng_bnd.getNorthEast().lat,
        nwx: lat_lng_bnd.getNorthEast().lng,
      }

      $.getJSON('/meshblocks_bounds', data, getMeshblockCallback)
        .fail(function () {
          $('#load').addClass('hidden')
        })
    }
    instruct.update()
  }

  map.on('moveend', function () {
    updateMap();
  });

  var legend = L.control({ position: 'bottomright' });

  legend.onAdd = function (map) {
    var div = L.DomUtil.create('div', 'legend');
    div.innerHTML = [
      '<i style="background:', claimStyles.claimed_by_you.fillColor, '"></i><div>My area</div>',
      '<i style="background:', claimStyles.claimed.fillColor, '"></i><div>Claimed</div>',
      '<i style="background:', claimStyles.quarantine.fillColor, '"></i><div>Organised door knocking event</div>',
      '<i style="background:', claimStyles.unclaimed.fillColor, '"></i><div>Yet to be claimed!</div>'
    ].join('');
    return div;
  }

  legend.addTo(map);
  map.whenReady(updateMap);
}



function windowHeight() {
  if (window.innerHeight != undefined) return window.innerHeight;
  var B = document.body, D = document.documentElement;
  return Math.max(B.clientHeight, D.clientHeight);
}
var headerHeight = $('.header').height();

var LAMBDA_BASE_URL = $("#map").data("lambda-base-url");
$('#map').height(windowHeight() - headerHeight);
$('#map').width("100%");
makeMap();


function openHelp() {
  $('#dialog').dialog({
    minWidth: 800,
    width: 800,
    height: Math.min(windowHeight() - headerHeight - 200, 800),
    beforeClose: function () { $('#dialog').addClass('hidden') }
  });
  $('#dialog').dialog({ position: { my: 'center top', at: 'center top+15%', of: window } });
  $('#dialog').removeClass('hidden');
  try { window.localStorage.setItem('helpSeen', 'true') } catch (_) { }
};
$('#faqlink').click(openHelp);

var helpSeen = 'false';
try { helpSeen = window.localStorage.getItem('helpSeen') } catch (_) { }
if (helpSeen !== 'true') $(window).load(openHelp);
