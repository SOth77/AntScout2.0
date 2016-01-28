(function() {

  require(["jquery", "styles", "bootstrap", "leaflet-src", "underscore"], function($, styles) {
    var addWaysToLayer, clearNodeData, clearWayData, cloudMadeAttribution, cloudMadeLayer, cloudMadeUrl, deselectNode, deselectWay, destination, destinationMarker, disable, displayNodeData, displayWayData, drawPath, enable, incomingWaysLayer, map, nodes, nodesLayer, osmLayer, outgoingWaysLayer, pathLayer, retrieveNode, retrieveNodes, retrievePath, retrieveWays, selectNode, selectWay, selectedNode, selectedWay, setNodeAsDestination, setNodeAsSource, showErrorMessage, source, sourceAndDestinationLayer, sourceMarker, toggleDisabledProperty, toggleWayEditMaxSpeedControls, ways, waysLayer;
    this.AntScout = {};
    cloudMadeUrl = 'http://{s}.tile.cloudmade.com/{apiKey}/{styleId}/256/{z}/{x}/{y}.png';
    cloudMadeAttribution = 'Map data &copy; 2011 OpenStreetMap contributors, Imagery &copy; 2011 CloudMade';
    cloudMadeLayer = L.tileLayer(cloudMadeUrl, {
      attribution: cloudMadeAttribution,
      apiKey: "396d772f0ece43f49d3801843fd86fbc",
      styleId: 998
    });
    destination = null;
    destinationMarker = null;
    pathLayer = new L.LayerGroup();
    incomingWaysLayer = new L.LayerGroup();
    map = null;
    nodes = null;
    nodesLayer = new L.LayerGroup();
    osmLayer = L.tileLayer("http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", {
      attribution: cloudMadeAttribution
    });
    outgoingWaysLayer = new L.LayerGroup();
    ways = null;
    waysLayer = new L.LayerGroup();
    selectedNode = null;
    selectedWay = null;
    source = null;
    sourceAndDestinationLayer = L.layerGroup();
    sourceMarker = null;
    clearNodeData = function() {
      $("#nodeId").html("");
      $("#nodeLongitude").html("");
      return $("#nodeLatitude").html("");
    };
    clearWayData = function() {
      $("#wayId").html("");
      $("#wayLength").html("");
      $("#wayMaxSpeed").html("");
      $("#wayTripTime").html("");
      return $("#way-nodes").html("");
    };
    deselectNode = function() {
      if (selectedNode != null) {
        clearNodeData();
        incomingWaysLayer.clearLayers();
        outgoingWaysLayer.clearLayers();
        selectedNode.setRadius(styles.node.radius);
        selectedNode.setStyle(styles.node);
        return selectedNode = null;
      }
    };
    deselectWay = function() {
      if (selectedWay != null) {
        clearWayData();
        selectedWay.setStyle(selectedWay.style);
        return selectedWay = null;
      }
    };
    displayNodeData = function(node) {
      $("#nodeId").html("<a href=\"http://www.openstreetmap.org/browse/node/" + node.id + "\">" + node.id + "</a>");
      $("#nodeLongitude").html(node.longitude);
      return $("#nodeLatitude").html(node.latitude);
    };
    retrieveNodes = function() {
      return $.get("nodes", function(nodes) {
        var marker, node, _i, _len;
        this.nodes = this;
        for (_i = 0, _len = nodes.length; _i < _len; _i++) {
          node = nodes[_i];
          marker = L.circleMarker([node.latitude, node.longitude], styles.node);
          marker.node = node;
          marker.addTo(nodesLayer);
          marker.on("click", function(e) {
            return selectNode(e.target);
          });
        }
        return map.fitBounds((function() {
          var _j, _len1, _results;
          _results = [];
          for (_j = 0, _len1 = nodes.length; _j < _len1; _j++) {
            node = nodes[_j];
            _results.push([node.latitude, node.longitude]);
          }
          return _results;
        })());
      });
    };
    retrieveWays = function() {
      return $.get("ways", function(ways) {
        var node, way;
        addWaysToLayer(ways, waysLayer, styles.way, styles.selectedWay);
        return map.fitBounds((function() {
          var _i, _len, _results;
          _results = [];
          for (_i = 0, _len = ways.length; _i < _len; _i++) {
            way = ways[_i];
            _results.push((function() {
              var _j, _len1, _ref, _results1;
              _ref = way.nodes;
              _results1 = [];
              for (_j = 0, _len1 = _ref.length; _j < _len1; _j++) {
                node = _ref[_j];
                _results1.push([node.latitude, node.longitude]);
              }
              return _results1;
            })());
          }
          return _results;
        })());
      });
    };
    selectNode = function(nodePath) {
      var shouldSelect;
      shouldSelect = !(selectedNode != null) || nodePath !== selectedNode;
      deselectNode();
      if (shouldSelect) {
        nodePath.setRadius(styles.selectedNode.radius);
        nodePath.setStyle(styles.selectedNode);
        selectedNode = nodePath;
        displayNodeData(selectedNode.node);
        return retrieveNode(selectedNode.node.id);
      }
    };
    selectWay = function(way) {
      var shouldSelect;
      shouldSelect = !(selectedWay != null) || way !== selectedWay;
      deselectWay();
      if (shouldSelect) {
        selectedWay = way;
        selectedWay.setStyle(selectedWay.selectedStyle);
        return displayWayData(way.way);
      }
    };
    $(function() {
      var baseLayers, overlayLayers;
      map = L.map("map", {
        layers: [pathLayer, nodesLayer, osmLayer, sourceAndDestinationLayer, waysLayer]
      }).fitWorld();
      baseLayers = {
        "CloudMade": cloudMadeLayer,
        "OpenStreetMap": osmLayer
      };
      overlayLayers = {
        "Incoming ways": incomingWaysLayer,
        "Outgoing ways": outgoingWaysLayer,
        "Ways": waysLayer,
        "Path": pathLayer,
        "Nodes": nodesLayer,
        "Source and destination": sourceAndDestinationLayer
      };
      L.control.layers(baseLayers, overlayLayers).addTo(map);
      L.control.scale().addTo(map);
      retrieveNodes();
      retrieveWays();
      $("#setNodeAsSource").click(function() {
        return setNodeAsSource();
      });
      $("#setNodeAsDestination").click(function() {
        return setNodeAsDestination();
      });
      $("#wayEditMaxSpeed, #waySaveMaxSpeed, #wayCancelEditMaxSpeed").click(function() {
        return toggleWayEditMaxSpeedControls();
      });
      $("#wayEditMaxSpeed").click(function() {
        return $("#wayMaxSpeed").select();
      });
      $("#waySaveMaxSpeed").click(function() {
        var maxSpeed;
        maxSpeed = parseFloat($("#wayMaxSpeedInput").val().replace(",", "."));
        return $.ajax({
          contentType: "application/json",
          type: "PUT",
          url: "way/" + selectedWay.way.id,
          data: JSON.stringify({
            maxSpeed: maxSpeed
          })
        }).done(function(way) {
          _.each([incomingWaysLayer, outgoingWaysLayer, pathLayer, waysLayer], function(layerGroup) {
            return layerGroup.eachLayer(function(layer) {
              if (layer.way.id === way.id) {
                return layer.way = way;
              }
            });
          });
          selectWay.way = way;
          return displayWayData(way);
        });
      });
      $("#pathLength, #pathTripTime, #wayLength, #wayMaxSpeed, #wayMaxSpeedInput, #wayTripTime").each(function() {
        return $(this).tooltip({
          trigger: 'hover'
        });
      });
      $("#trace").click(function() {
        var $this;
        $this = $(this);
        if (!$this.hasClass("active")) {
          return $.ajax({
            contentType: "application/json",
            type: "PUT",
            url: "debug/trace"
          });
        } else {
          return $.ajax({
            contentType: "application/json",
            type: "DELETE",
            url: "debug/trace"
          });
        }
      });
      $("#show-node-additional-data").click(function() {
        return $("#node-additional-data").collapse("toggle");
      });
      return $("#show-path-additional-data").click(function() {
        return $("#path-additional-data").collapse("toggle");
      });
    });
    addWaysToLayer = function(ways, layer, style, selectedStyle) {
      var node, polyline, way, _i, _len, _results;
      _results = [];
      for (_i = 0, _len = ways.length; _i < _len; _i++) {
        way = ways[_i];
        polyline = L.polyline((function() {
          var _j, _len1, _ref, _results1;
          _ref = way.nodes;
          _results1 = [];
          for (_j = 0, _len1 = _ref.length; _j < _len1; _j++) {
            node = _ref[_j];
            _results1.push([node.latitude, node.longitude]);
          }
          return _results1;
        })(), style);
        polyline.selectedStyle = selectedStyle;
        polyline.style = style;
        polyline.way = way;
        polyline.addTo(layer);
        _results.push(polyline.on("click", function(e) {
          return selectWay(e.target);
        }));
      }
      return _results;
    };
    disable = function(elementId) {
      return $("#" + elementId).prop("disabled", true);
    };
    displayWayData = function(way) {
      var length, lengths, maxSpeed, maxSpeeds, node, tripTime, tripTimes;
      $("#wayId").html(way.id);
      lengths = (function() {
        var _i, _len, _ref, _results;
        _ref = way.lengths;
        _results = [];
        for (_i = 0, _len = _ref.length; _i < _len; _i++) {
          length = _ref[_i];
          _results.push("" + length.value + " " + length.unit);
        }
        return _results;
      })();
      $("#wayLength").attr("data-original-title", lengths.join("<br>")).html(way.length);
      maxSpeeds = (function() {
        var _i, _len, _ref, _results;
        _ref = way.maxSpeeds;
        _results = [];
        for (_i = 0, _len = _ref.length; _i < _len; _i++) {
          maxSpeed = _ref[_i];
          _results.push("" + maxSpeed.value + " " + maxSpeed.unit);
        }
        return _results;
      })();
      $("#wayMaxSpeed").attr("data-original-title", maxSpeeds.join("<br>")).html(way.maxSpeed);
      tripTimes = (function() {
        var _i, _len, _ref, _results;
        _ref = way.tripTimes;
        _results = [];
        for (_i = 0, _len = _ref.length; _i < _len; _i++) {
          tripTime = _ref[_i];
          _results.push("" + tripTime.value + " " + tripTime.unit);
        }
        return _results;
      })();
      $("#wayTripTime").attr("data-original-title", tripTimes.join("<br>")).html(way.tripTime);
      nodes = (function() {
        var _i, _len, _ref, _results;
        _ref = way.nodes;
        _results = [];
        for (_i = 0, _len = _ref.length; _i < _len; _i++) {
          node = _ref[_i];
          _results.push("<a href=\"http://www.openstreetmap.org/browse/node/" + node.id + "\">" + node.id + "</a>");
        }
        return _results;
      })();
      return $("#way-nodes").html("<ul><li>" + nodes.join("</li><li>") + "</li></ul>");
    };
    drawPath = function(path) {
      var length, lengths, tripTime, tripTimes;
      pathLayer.clearLayers();
      if (path && (path.ways != null) && path.ways.length > 0) {
        lengths = (function() {
          var _i, _len, _ref, _results;
          _ref = path.lengths;
          _results = [];
          for (_i = 0, _len = _ref.length; _i < _len; _i++) {
            length = _ref[_i];
            _results.push("" + length.value + " " + length.unit);
          }
          return _results;
        })();
        $("#pathLength").attr("data-original-title", lengths.join("<br>")).html(path.length);
        tripTimes = (function() {
          var _i, _len, _ref, _results;
          _ref = path.tripTimes;
          _results = [];
          for (_i = 0, _len = _ref.length; _i < _len; _i++) {
            tripTime = _ref[_i];
            _results.push("" + tripTime.value + " " + tripTime.unit);
          }
          return _results;
        })();
        $("#pathTripTime").attr("data-original-title", tripTimes.join("<br>")).html(path.tripTime);
        return addWaysToLayer(path.ways, pathLayer, styles.path, styles.selectedPath);
      }
    };
    AntScout.path = function(path) {
      return drawPath(path);
    };
    AntScout.way = function(way, newSpeed) {
      var maxSpeed;
      maxSpeed = parseFloat(newSpeed);
      return $.ajax({
        contentType: "application/json",
        type: "PUT",
        url: "way/" + way.id,
        data: JSON.stringify({
          maxSpeed: maxSpeed
        })
      }).done(function(way) {
        return _.each([incomingWaysLayer, outgoingWaysLayer, pathLayer, waysLayer], function(layerGroup) {
          return layerGroup.eachLayer(function(layer) {
            if (layer.way.id === way.id) {
              return layer.way = way;
            }
          });
        });
      });
    };
    enable = function(elementId) {
      return $("#" + elementId).prop("disabled", false);
    };
    retrieveNode = function(id) {
      return $.get("node/" + id, function(nodeData) {
        incomingWaysLayer.clearLayers();
        addWaysToLayer(nodeData.incomingWays, incomingWaysLayer, styles.incomingWay, styles.selectedIncomingWay);
        outgoingWaysLayer.clearLayers();
        return addWaysToLayer(nodeData.outgoingWays, outgoingWaysLayer, styles.outgoingWay, styles.selectedOutgoingWay);
      });
    };
    retrievePath = function(source, destination) {
      return $.get("/path/" + source.id + "/" + destination.id, function(path) {
        return drawPath(path);
      });
    };
    setNodeAsDestination = function() {
      destination = selectedNode.node;
      if (!(destinationMarker != null)) {
        if (destinationMarker == null) {
          destinationMarker = L.marker([destination.latitude, destination.longitude], {
            icon: L.icon({
              iconAnchor: [14, 43],
              iconSize: [28, 43],
              iconUrl: "images/markers/green/B.png"
            })
          }).addTo(sourceAndDestinationLayer);
        }
      } else {
        destinationMarker.setLatLng([destination.latitude, destination.longitude]);
        destinationMarker.update();
      }
      if ((source != null) && (destination != null)) {
        return retrievePath(source, destination);
      }
    };
    setNodeAsSource = function() {
      source = selectedNode.node;
      if (!(sourceMarker != null)) {
        if (sourceMarker == null) {
          sourceMarker = L.marker([source.latitude, source.longitude], {
            icon: L.icon({
              iconAnchor: [14, 43],
              iconSize: [28, 43],
              iconUrl: "images/markers/green/A.png"
            })
          }).addTo(sourceAndDestinationLayer);
        }
      } else {
        sourceMarker.setLatLng([source.latitude, source.longitude]);
        sourceMarker.update();
      }
      if ((source != null) && (destination != null)) {
        return retrievePath(source, destination);
      }
    };
    showErrorMessage = function(message) {
      $("#error > p").html(message);
      $("#error").show().delay(5000).fadeOut("slow");
      return $("#error > p").html();
    };
    toggleDisabledProperty = function(elementId) {
      if ($("#" + elementId).prop("disabled") === true) {
        return enable(elementId);
      } else {
        return disable(elementId);
      }
    };
    return toggleWayEditMaxSpeedControls = function() {
      if ($("#wayMaxSpeed").is(":visible")) {
        $("#wayMaxSpeed").hide();
        $("#wayMaxSpeedInput").attr("data-original-title", $("#wayMaxSpeed").attr("data-original-title")).show().val($("#wayMaxSpeed").html()).select();
      } else {
        $("#wayMaxSpeedInput").hide();
        $("#wayMaxSpeed").show();
      }
      toggleDisabledProperty("wayEditMaxSpeed");
      toggleDisabledProperty("waySaveMaxSpeed");
      return toggleDisabledProperty("wayCancelEditMaxSpeed");
    };
  });

}).call(this);
