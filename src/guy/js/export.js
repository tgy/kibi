$(document).ready(function() {

  $("#export-button").click(function() {
    var result = "";
    $("#result > span > input").each(function() {
      result += $(this).val() + " ";
    });
    alert(result);
  });

});
