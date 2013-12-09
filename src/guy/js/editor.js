$(document).ready(function() {
/*
  var path = "data/robert.json";

  $.get(path, function (data) {
    data = JSON.parse(data);

    var tmp = "";

    for (p in data.content) {
      for (l in data.content[p]) {
        for (w in data.content[p][l]) {
          for (k in data.content[p][l][w]) {
            tmp += '<span';
            if (data.content[p][l][w][k].length > 0)
              tmp += ' class="error"';
            tmp += '><input type="text" value="' + k + '" />';

            if (data.content[p][l][w][k].length > 0) {
              tmp += '<div class="correctionlist"><ul>';
              for (corr in data.content[p][l][w][k]) {
                tmp += '<li>' + data.content[p][l][w][k][corr] + '</li>';
              }
              tmp += '</ul></div>';
              
            }

            tmp += '</span> ';
          }
        }
      }
      tmp += "</br></br>";

    }
    $("#result").html(tmp);
    console.log(tmp);
  });
*/ 
  function adjustWidth(input) {
      var charWidth = 7;
      var newWidth = (input.val().length) * charWidth;
      input.css("width", newWidth + "px");
      input.parent().css("width", (newWidth + 1) + "px");
  }

  function hideCorrection(correctionlist) {
      if (correctionlist.is(":visible"))
          correctionlist.hide();
  }

  function showCorrection(correctionlist) {
      $(".correctionlist").hide();
      if (correctionlist.is(":hidden"))
          correctionlist.show();
  }

  function nextFocus(span) {
      var nextspan = span.next("span.error");
      nextspan.focus();
      showCorrection(nextspan.find(".correctionlist"));
  }

  function setCorrected(span) {
      if (!span.hasClass("corrected"))
          span.addClass("corrected");
  }

  // var firstError = $("#result span.error").eq(0).find("input");

  // firstError.focus();
  // showCorrection(firstError.next(".correctionlist"));

  // $("html").click(function() {
  //     $("#result span.error").each(function() {
  //         hideCorrection($(this).find(".correctionlist"));
  //     });

  //     $("#result span.corrected").each(function() {
  //         hideCorrection($(this).find(".correctionlist"));
  //     });
  // });


  $("#result span > input").keyup(function(e) {
      var code = e.keyCode || e.which;
      if (code == 27)
      {
          $(this).blur();
      }
      else if (code == 9)
      {
          console.log("tab");
          $(this).parent().parent().next(".error").hide();
      }
      else
      {
          adjustWidth($(this));
      }
  });

  $("#result span > input").change(function() {
      adjustWidth($(this));
      setCorrected($(this).parent());
  });

  $("#result span > input").click(function() {
      $(this).select();
  });

  var lastClick = null;
  $(".correctionlist > ul > li").mousedown(function(e) {
      var input = $(this).parent().parent().parent().find("input");
      input.val($(this).html());
      adjustWidth(input);
      setCorrected(input.parent());
      $(this).parent().parent().hide();
  });

  $("#result span.error > input").focusin(function(e) {
      showCorrection($(this).next(".correctionlist"));
  });

  $("#result span.error > input").focusout(function(e) {
      hideCorrection($(this).next(".correctionlist"));
  });

  $("#result span > input").each(function() {
      adjustWidth($(this));
  });

});
