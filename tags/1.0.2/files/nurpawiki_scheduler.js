
function nwSetButtonCheckedState(v)
{
    var inputs = document.getElementsByTagName("input");
    var re = new RegExp("^t-([0-9]+)$");

    for (var i = 0; i < inputs.length; i++)
    {
        var m = re.exec(inputs[i].name);
        if (m != null)
        {
            inputs[i].checked = v;
        }
    }
}

function nwSelectAllButton()
{
    nwSetButtonCheckedState(true);
}

function nwDeselectAllButton()
{
    nwSetButtonCheckedState(false);
}

function nwRegisterSchedulerButtons()
{
    var b = document.getElementById("button_select_all");
    b.onclick = nwSelectAllButton;

    var b = document.getElementById("button_deselect_all");
    b.onclick = nwDeselectAllButton;
}

addLoadEvent(nwRegisterSchedulerButtons);
