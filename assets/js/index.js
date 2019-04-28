$(document).ready(function() {
    switchPosts("writingCategory",  "projectCategory", "projectCategory", "artCategory")
    switchActiveButton("writingButton", "projectButton", "projectButton", "artButton")

  $("#writingButton").click(function() {
    switchPosts("writingCategory",  "projectCategory", "projectCategory", "artCategory")
    switchActiveButton("writingButton", "projectButton", "projectButton", "artButton")
  });
  $("#projectButton").click(function() {
    switchPosts("projectCategory",  "writingCategory", "projectCategory", "artCategory")
    switchActiveButton("projectButton", "writingButton", "projectButton", "artButton")
  });
  $("#artButton").click(function() {
    switchPosts("artCategory",  "writingCategory", "projectCategory", "projectCategory")
    switchActiveButton("artButton", "writingButton", "projectButton", "projectButton")
  });
  $("#projectButton").click(function() {
    switchPosts("projectCategory",  "writingCategory", "projectCategory", "artCategory")
    switchActiveButton("projectButton", "writingButton", "projectButton", "artButton")
  });
});

function switchPosts() {
  var postToShow = document.getElementById(arguments[0]);
  postToShow.style.display = "block";
  for (var i = 1; i < arguments.length; i++) {
    var postToHide = document.getElementById(arguments[i]);
    postToHide.style.display = "none";
  }
}

function switchActiveButton() {
  var activeButton = document.getElementById(arguments[0]);
  activeButton.style.opacity = 1;
  for (var i = 1; i < arguments.length; i++) {
    var inactiveButton = document.getElementById(arguments[i]);
    inactiveButton.style.opacity = 0.5;
  }
}
