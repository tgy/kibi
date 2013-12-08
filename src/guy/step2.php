<?php include 'head.php'; ?>

<?php 

if (isset($_FILES["image"])) {

}
else {
    $error = 'No image sent. <a href="step1.php">Come back</a>';
}

?>

<div id="step-number"><div id="img" class="step2"></div></div>

<?php if (!isset($error)) { ?>

<div id="processed"></div>

<?php } else { 

echo '<div class="errorinfo">' . $error . '</div>' ;

} ?>

<script src="js/processing.js"></script>

<?php include 'foot.php'; ?>