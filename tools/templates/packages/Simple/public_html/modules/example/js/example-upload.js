
$(document).ready( function () {
    $("#form-with-ajax").submit(function(e){
        e.preventDefault();
        $("#upload-ajax-note").html('');
        $("#upload-ajax-note").removeClass('bg-danger text-white');
        var formData = new FormData(this);
        url = $("#form-with-ajax").prop('action');

        $.ajax({
            url: url,
            type: 'POST',
            data: formData,
            success: function (data) {
                $("#form-with-Ajax").trigger("reset");
                if (data.code === 0){
                    alert("Terima kasih, informasi telah disimpan.");
                }else{
                    $("#upload-ajax-note").html(data.msg);
                    $("#upload-ajax-note").addClass('bg-danger text-white');
                }
            },
            cache: false,
            contentType: false,
            processData: false
        });
        return false;
    });
});
