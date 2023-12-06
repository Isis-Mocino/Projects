using GuiaTuristica.Models;
using Newtonsoft.Json;
using System.Collections.ObjectModel;
using System.Net.Http.Headers;

namespace GuiaTuristica
{
    public partial class MainPage : ContentPage
    {
        HttpClient client = new HttpClient();

        public ObservableCollection<Lugar> Lugares { get; set; }
        public MainPage()
        {
            InitializeComponent();
            ObtenerLugares();
            BindingContext = this;
        }

        private async void ObtenerLugares()
        {
            ObservableCollection<Lugar> lugaresAPI = new ObservableCollection<Lugar>();

            string url = "https://guiaturistica.azurewebsites.net/api/lugarea";

            client.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue("bearer", await SecureStorage.GetAsync("token"));

            var respuesta = client.GetAsync(url);
            if (!respuesta.Result.IsSuccessStatusCode)
            {
                await DisplayAlert("Error", "No se pudo obtener los datos", "Ok");
                return;
            }
            else
            {
                var json = await respuesta.Result.Content.ReadAsStringAsync();
                lugaresAPI = JsonConvert.DeserializeObject<ObservableCollection<Lugar>>(json);
                if (lugaresAPI.Count == 0)
                {
                    await DisplayAlert("Menaje", "No existen datos para mostrar", "Ok");
                }
            }
        }
    }

}
