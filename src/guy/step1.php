<?php include 'head.php'; ?>

<div id="step-number"><div id="img" class="step1"></div></div>

<form action="step2.php">
    <input type="file" name="image" id="image-browser">
    <button id="choose-file">Choose an image to process</button>
</form>

<script src="js/browse.js"></script>

<?php include 'foot.php'; ?>