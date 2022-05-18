$(document).contextMenu({
    selector: '#dtable .field',
    trigger: 'right',
    items: {
        // <input type="radio">
        menuHeading1: {
          type: 'html',
          html: '<h4><strong>Match header with one of these:</strong></h4>'
        },
        Date: {
            name: "Date", 
            type: 'radio', 
            radio: 'radio', 
            value: 'Date',
        },
        SiteID: {
            name: "SiteID", 
            type: 'radio', 
            radio: 'radio', 
            value: 'SiteID',
        },
        Penetration: {
            name: "Penetration", 
            type: 'radio', 
            radio: 'radio', 
            value: 'Penetration'
        },
        SpeciesCode: {
            name: "Species code", 
            type: 'radio', 
            radio: 'radio', 
            value: 'SpeciesCode'
        },
        Altitude: {
            name: "Altitude", 
            type: 'radio', 
            radio: 'radio', 
            value: 'Altitude'
        },
        sep1: "---------",
        // <optional>
        menuHeading2: {
          type: 'html',
          html: '<h4><strong>Optional location</strong> (needed for map)</h4>'
        },
        Easting: {
            name: "Easting (opt)", 
            type: 'radio', 
            radio: 'radio', 
            value: 'Easting'
        },
        Northing: {
            name: "Northing (opt)", 
            type: 'radio', 
            radio: 'radio', 
            value: 'Northing'
        },
	sep2: "---------",
	None: {
	    name: "[Remove]",
	    type: 'radio',
	    radio: 'radio',
	    value: 'None'
	}
    },
  events: {
    show: function(opt) {
      var $this = this;
      $.contextMenu.setInputValues(opt, $this.data());
    },
    hide: function(opt) {
      var $this = this;
      var data = $.contextMenu.getInputValues(opt, $this.data());
      var $th = opt.$trigger;
      if (typeof data.radio !== "undefined") {
        Shiny.onInputChange("selfield", data.radio + "=" + $th.text());
      }
    }
  }
});
// table.columns.adjust().draw();
// table.columns.adjust().draw();
// table.columns.adjust().draw();

// Shiny.addCustomMessageHandler('refr', refreshTable);
// function refreshTable() {
//     $('#dtable').columns.adjust().draw();
//     table.columns.adjust().draw();
//     table.css('background-color', '#f00');
//     $('#dtable').css('background-color', '#f00');
//     alert('test alert');
// }
// refreshTable();

// var txt = 'Date';
// var txt2 = 'parane';
// var column = $('#dtable tr th').filter(function() {
//     return $(this).text() === txt;
// }).index();
// // alert( $('#dtable tr').find('th').eq(1).attr('colspan'));
// var count=$('#dtable tr').find('th').eq(column).attr('colspan');
// // alert(count);
// if(column > -1) {
//     $('#dtable tr').each(function() {
// 	var trObj=$(this);
// //alert(trObj);
//         $(this).find('td').eq(column).css('background-color', '#ccc');
// 	for(var lpvar=column;lpvar<=count;lpvar++)
// 	{
// 	    //alert(trObj.find('td').eq(lpvar).text());
// 	    trObj.find('td').eq(lpvar).css('background-color', '#ccc');
// 	}
//     });
// }

// if(column > -1) {
//     $('#dtable tr').each(function() {
//         $(this).find('td').eq(column).css('background-color', '#f00');
//     });
// }


// function applyBackground(txt) {

//   var column = $('#dtable tr th:contains("' + txt + '")');
//   if (column.length < 1) {
//     return;
//   }
//   var getColspans = column.attr("colspan");
//   var columnValue = column.index();


//   var totalCells = 0;
//   $.each(column.siblings(), function(value, ele) {
//     //Only count the cells that are prior to the selected Column
//     if (value < columnValue) {
//       totalCells = totalCells + parseInt($(ele).attr("colspan"))
//     }
//   });


//   $('#dtable tbody tr').each(function() {
//     for (let idx = 0; idx < parseInt(getColspans); idx++) {
//       $(this).find('td').eq(totalCells + idx).css('background-color', '#f00');
//     }
//   });

// }

// applyBackground('Date');
