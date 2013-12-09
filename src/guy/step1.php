<?php include 'head.php'; ?>

<div id="step-number"><div id="img" class="step1"></div></div>

<form id="step1-form" action="step2.php" method="POST"
      enctype="multipart/form-data">
    <input type="file" name="image" id="image-browser">
    <div id="choose-file">Choose an image to process</div>
    <input id="start-process" type="submit" value="Start process">
</form>

<script src="js/browser.js"></script>

<?php include 'foot.php'; ?>