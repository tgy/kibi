$(document).ready(function () {

    $("#choose-file").click(function (e) {
        $("#image-browser").click();
        e.stopPropagation();
    });

    $("#step1-form").submit(function (event) {
        if ( $("#image-browser").val() == "" ) {
            alert("Please choose an image to process");
            event.preventDefault();
        }
    });

});