using GuiaTuristica.Models;
using Newtonsoft.Json;
using System.Text;

namespace GuiaTuristica.Views;

public partial class RegistroPage : ContentPage
{
    private readonly HttpClient client = new HttpClient();
    public RegistroPage()
	{
		InitializeComponent();
	}

    private async void btnRegistro_Clicked(object sender, EventArgs e)
    {
        string url = "https://guiaturistica.azurewebsites.net/api/Cuentas/registro";

        User user = new User
        {
            UserName = txtUserName.Text,
            Email = txtEmail.Text,
            Password = txtPassword.Text
        };

        string jsonUser = JsonConvert.SerializeObject(user);

        StringContent content = new StringContent(jsonUser, Encoding.UTF8, "application/json");

        var respuesta = await client.PostAsync(url, content);

        var tokenString = respuesta.Content.ReadAsStringAsync();

        var json = JsonConvert.DeserializeObject<UserToken>(tokenString.Result);

        if (respuesta.IsSuccessStatusCode)
        {
            await SecureStorage.SetAsync("token", json.Token);
            await Navigation.PushAsync(new MainPage());
        }
        else
        {
            await DisplayAlert("Error", "No se pudo registrar", "Ok");
        }
    }
}