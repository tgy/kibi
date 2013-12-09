$(document).ready(function () {

    var lastLineIndex = -1;
    var started = false;
    var stopped = false;

    function addProcess(processName) {
        $("#processed").append(
            '<span class="process loading">$ ' + processName +'</span>'
        );
    }

    function addTime(time) {
        var last = $("#processed > span.process:last-child");
        last.removeClass("loading");
        last.append(" • " + parseFloat(time));
    }

    function refresh () {
        $.get( "data/processing.tmp", function(data) {
            var lines = data.split("\n");

            if (lines.length > 0) {

                if (lastLineIndex >= lines.length - 2
                    && lines[lines.length - 1] == "END") {
                    if (!stopped) {
                        $("#processed").append(
                            '<a href="step3.php" id="render">Check it out!</a>');
                    }
                    stopped = true;
                    return;
                }

                if (!started && lastLineIndex < 0) {
                    $("#processed").append(
                        '<span class="process">$ Program started ...</span>'
                    );
                    started = true;
                }

                else if (lines.length > lastLineIndex + 1) {

                    if (lastLineIndex % 2 == 0) {
                        addTime(lines[lastLineIndex + 1]);
                    }

                    else {
                        addProcess(lines[lastLineIndex + 1]);
                    }
                    lastLineIndex++;
                }
            }
        });
    }

    setInterval(refresh, 100);
});


// <a href="step3.php" id="render">Finished!</a>